.mdb_study <- function(washout, drug = 1503297) {
  omop_study(omop_new_users(drug, washout_days = washout), name = "base",
             baseline = list(age = omop_age()), outputs = "baseline")
}

.mdb_same <- function(actual, expected) {
  a <- recipe_to_plan(actual)
  b <- recipe_to_plan(expected)
  expect_identical(a, b)
  expect_identical(dsOMOPClient:::.plan_v2_digest(a)$sha256,
                   dsOMOPClient:::.plan_v2_digest(b)$sha256)
}

.mdb_roundtrips <- function(r) {
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(r)), envir = env)
  out <- list(r, env$recipe, recipe_import_json(recipe_export_json(r)))
  if (requireNamespace("yaml", quietly = TRUE)) {
    out <- c(out, list(recipe_import_yaml(recipe_export_yaml(r))))
  }
  out
}

test_that("deferred shared bases commute through zero and positive washout binding", {
  for (days in c(0L, 30L)) {
    a <- .mdb_study(days)
    b <- .mdb_study(omop_param("washout", "integer", default = days))
    merged <- recipe_merge(a, b, prefix = "b")
    expected <- recipe_merge(a, recipe_bind(b, washout = days), prefix = "b")
    expect_identical(names(merged$populations), c("base", "b_base"))
    for (r in .mdb_roundtrips(merged)) {
      .mdb_same(r, expected)
      bound <- recipe_bind(r, washout = days)
      expect_identical(names(bound$populations), "base")
      expect_identical(bound$outputs$b_baseline$population_id, "base")
      .mdb_same(bound, expected)
    }
    # An explicit override must retain a genuinely different base.
    other <- if (days == 0L) 30L else 0L
    different <- recipe_bind(merged, washout = other)
    expect_identical(names(different$populations), c("base", "b_base"))
    .mdb_same(different, recipe_merge(a, recipe_bind(b, washout = other), prefix = "b"))
  }
})

test_that("staged and nested base reconciliation preserves current namespaces", {
  a <- .mdb_study(0L)
  w <- omop_param("first", "integer") + omop_param("second", "integer")
  b <- .mdb_study(w)
  pending <- recipe_merge(a, b, prefix = "b")
  staged <- recipe_bind(pending, first = 0L)
  expect_identical(names(staged$populations), c("base", "b_base"))
  expected <- recipe_merge(a, recipe_bind(b, first = 0L, second = 0L), prefix = "b")
  for (r in .mdb_roundtrips(staged)) .mdb_same(recipe_bind(r, second = 0L), expected)

  outer <- recipe_merge(omop_recipe(), staged, prefix = "outer")
  outer_expected <- recipe_merge(omop_recipe(), expected, prefix = "outer")
  for (r in .mdb_roundtrips(outer)) {
    bound <- recipe_bind(r, second = 0L)
    expect_identical(names(bound$populations), c("base", "outer_base"))
    expect_identical(bound$outputs$outer_b_baseline$population_id, "outer_base")
    .mdb_same(bound, outer_expected)
  }
})

test_that("post-merge population and output edits survive deferred reconciliation", {
  a <- .mdb_study(0L)
  b <- .mdb_study(omop_param("washout", "integer", default = 0L))
  pending <- recipe_merge(a, b, prefix = "b")
  for (id in c("base", "b_base")) {
    edited <- pending
    edited$populations[[id]]$filters <- list(omop_observed(before = 12L))
    for (r in .mdb_roundtrips(edited)) {
      bound <- recipe_bind(r, washout = 0L)
      expect_identical(names(bound$populations), c("base", "b_base"))
      expect_identical(bound$populations[[id]]$filters, edited$populations[[id]]$filters)
    }
  }

  # Added references follow a collapse; removed objects are never replayed.
  pending$outputs$extra <- omop_output("extra", rows = "b_base", format = "wide", columns = "b_age")
  pending$outputs$b_baseline <- NULL
  pending$populations$child <- omop_population("child", parent_id = "b_base")
  pending$populations$either <- omop_population("either", union = c("child", "b_base"))
  pending$blocks$dx <- omop_variable_block("dx", "condition_occurrence", 1,
                                          population_id = "b_base")
  bound <- recipe_bind(pending, washout = 0L)
  expect_null(bound$outputs$b_baseline)
  expect_identical(bound$outputs$extra$population_id, "base")
  expect_identical(bound$populations$child$parent_id, "base")
  expect_identical(bound$populations$either$setop$members, c("child", "base"))
  expect_identical(bound$blocks$dx$population_id, "base")
  removed <- pending
  removed$populations$base <- NULL
  expect_false("base" %in% names(recipe_bind(removed, washout = 0L)$populations))
  distinct <- pending
  distinct$populations$either <- omop_population("either", union = c("base", "b_base"))
  expect_true("b_base" %in% names(recipe_bind(distinct, washout = 0L)$populations))
})

.mdb_named <- function(washout, concept = 1503297) {
  r <- .mdb_study(washout, omop_event("drug_exposure", "drug", name = "rx"))
  r$concept_sets <- list(drug = omop_concept_set(concept, name = "drug"))
  r
}

test_that("named source declarations are verified transitively before bases share", {
  a <- .mdb_named(0L)
  b <- .mdb_named(omop_param("washout", "integer", default = 0L))
  expected <- recipe_merge(a, recipe_bind(b, washout = 0L), prefix = "b")
  expect_identical(names(expected$populations), "base")
  pending <- recipe_merge(a, b, prefix = "b")
  for (r in .mdb_roundtrips(pending)) .mdb_same(recipe_bind(r, washout = 0L), expected)

  for (name in c("drug", "b_drug")) {
    edited <- pending
    edited$concept_sets[[name]] <- omop_concept_set(2, name = name)
    bound <- recipe_bind(edited, washout = 0L)
    expect_identical(names(bound$populations), c("base", "b_base"))
    expect_identical(bound$concept_sets[[name]], edited$concept_sets[[name]])
  }
  # Equal text in a source reference does not make different declarations equal.
  different <- recipe_merge(a, .mdb_named(0L, 2), prefix = "b")
  expect_identical(names(different$populations), c("base", "b_base"))
  .mdb_same(recipe_bind(recipe_merge(a,
    .mdb_named(omop_param("washout", "integer", default = 0L), 2), prefix = "b"),
    washout = 0L), different)
})

test_that("late named bindings and nested aliases use current source definitions", {
  cs <- omop_param("drug", "concept_set")
  literal <- omop_concept_set(1503297, name = "new_drug")
  a <- .mdb_study(0L, literal)
  b <- .mdb_study(omop_param("washout", "integer", default = 0L), cs)
  pending <- recipe_merge(a, b, prefix = "b")
  expected <- recipe_merge(a, recipe_bind(b, washout = 0L, drug = literal), prefix = "b")
  for (r in .mdb_roundtrips(pending)) {
    .mdb_same(recipe_bind(r, washout = 0L, drug = literal), expected)
  }
  outer <- recipe_merge(omop_recipe(), pending, prefix = "outer")
  target <- recipe_merge(omop_recipe(), expected, prefix = "outer")
  for (r in .mdb_roundtrips(outer)) {
    .mdb_same(recipe_bind(r, washout = 0L, drug = literal), target)
  }
})

test_that("collapsing a base preserves still-unbound source and window occurrences", {
  window <- omop_window(omop_bound(omop_anchor("lab"), -7L), 0L)
  for (type in c("event", "string")) {
    add <- function(r) {
      r$events <- list(lab = omop_event("measurement",
        omop_param("concepts", "concept_set"), name = "lab"))
      r$variables$n <- omop_variable(event = omop_param("source", type),
        reducer = omop_reduce("count", window = omop_param("window", "window")))
      r$outputs$baseline$variables <- c("age", "n")
      r$outputs$baseline$population_id <- omop_param("rows", "string", default = "base")
      r
    }
    a <- add(.mdb_study(0L))
    b <- add(.mdb_study(omop_param("washout", "integer")))
    pending <- recipe_merge(a, b, prefix = "b")
    staged <- recipe_bind(pending, washout = 0L)
    expect_identical(names(staged$populations), "base")
    concepts <- omop_concept_set(1, name = "lab_codes")
    expected <- recipe_merge(recipe_bind(a, source = "lab", window = window, concepts = concepts, rows = "base"),
      recipe_bind(b, washout = 0L, source = "lab", window = window, concepts = concepts, rows = "base"), prefix = "b")
    for (r in .mdb_roundtrips(staged)) {
      .mdb_same(recipe_bind(r, source = "lab", window = window, concepts = concepts, rows = "base"), expected)
      still_pending <- recipe_bind(r, source = "lab")
      for (partial in .mdb_roundtrips(still_pending)) {
        .mdb_same(recipe_bind(partial, window = window, concepts = concepts, rows = "base"), expected)
      }
    }
  }
})


test_that("base aliases wait for parameters in referenced registry declarations", {
  make <- function(concepts) {
    omop_recipe(events = list(omop_event("drug_exposure", concepts, name = "rx")),
      populations = omop_population("base", index = omop_index("rx")),
      variables = list(age = omop_age()),
      outputs = omop_output("ages", rows = "base", format = "wide"))
  }
  set <- omop_concept_set(1503297, name = "drug")
  a <- make(set)
  b <- make(omop_param("drug", "concept_set", default = set))
  pending <- recipe_merge(a, b, prefix = "b")
  expect_identical(names(pending$populations), c("base", "b_base"))
  expected <- recipe_merge(a, recipe_bind(b, drug = set), prefix = "b")
  expect_identical(names(expected$populations), "base")
  for (r in .mdb_roundtrips(pending)) {
    .mdb_same(r, expected)
    .mdb_same(recipe_bind(r, drug = set), expected)
    changed <- recipe_bind(r, drug = omop_concept_set(2, name = "drug"))
    expect_identical(names(changed$populations), c("base", "b_base"))
  }
})
