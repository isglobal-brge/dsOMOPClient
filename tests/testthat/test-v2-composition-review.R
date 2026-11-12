# A-23 / B6-B7: composition must preserve both emitted plans and semantic digests.
.cmp_same <- function(actual, expected) {
  a <- recipe_to_plan(actual)
  b <- recipe_to_plan(expected)
  expect_identical(a, b)
  expect_identical(dsOMOPClient:::.plan_v2_digest(a)$sha256,
                   dsOMOPClient:::.plan_v2_digest(b)$sha256)
}

.cmp_roundtrips <- function(recipe) {
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(recipe)), envir = env)
  out <- list(original = recipe, json = recipe_import_json(recipe_export_json(recipe)),
              code = env$recipe)
  if (requireNamespace("yaml", quietly = TRUE)) {
    out$yaml <- recipe_import_yaml(recipe_export_yaml(recipe))
  }
  out
}

.cmp_event_recipe <- function(id, source = omop_param("source", type = "event", default = "ev"),
                              window = NULL) {
  omop_recipe(events = list(omop_event("measurement", id, name = "ev")),
    variables = list(v = omop_variable(event = source,
      reducer = omop_reduce("count", window = window))),
    outputs = omop_output("w", format = "wide"))
}

test_that("deferred event defaults and bindings keep each occurrence's source namespace", {
  a <- .cmp_event_recipe(2)
  b <- .cmp_event_recipe(1)
  m <- recipe_merge(a, b, prefix = "b")
  expected <- recipe_merge(recipe_bind(a, source = "ev"), recipe_bind(b, source = "ev"), prefix = "b")
  expect_identical(names(recipe_params(m)), "source")
  for (r in .cmp_roundtrips(m)) {
    .cmp_same(r, expected)
    .cmp_same(recipe_bind(r, source = "ev"), expected)
    p <- recipe_to_plan(r)
    expect_identical(p$variables$v$source, list(event = "ev"))
    expect_identical(p$variables$b_v$source, list(event = "b_ev"))
  }
  # No first-recipe declaration is needed for a deferred source to resolve.
  .cmp_same(recipe_merge(omop_recipe(), b, prefix = "b"),
            recipe_merge(omop_recipe(), recipe_bind(b, source = "ev"), prefix = "b"))
})

test_that("window anchors and source maps survive staged binding and nested merges", {
  w <- omop_window(omop_bound(omop_anchor("ev", which = "first"), -10L), 0L)
  tar <- omop_param("window", type = "window", default = w)
  a <- .cmp_event_recipe(2, window = tar)
  b <- .cmp_event_recipe(1, window = tar)
  m <- recipe_merge(a, b, prefix = "b")
  target <- recipe_merge(recipe_bind(a, source = "ev", window = w),
                          recipe_bind(b, source = "ev", window = w), prefix = "b")
  for (r in .cmp_roundtrips(recipe_bind(m, source = "ev"))) {
    .cmp_same(r, target)
    .cmp_same(recipe_bind(r, window = w), target)
  }
  nested <- recipe_merge(omop_recipe(), m, prefix = "outer")
  nested_target <- recipe_merge(omop_recipe(), target, prefix = "outer")
  for (r in .cmp_roundtrips(nested)) {
    .cmp_same(r, nested_target)
    .cmp_same(recipe_bind(r, source = "ev", window = w), nested_target)
  }
})

test_that("registry concept defaults preserve shared definitions and local namespaces", {
  mk <- function(id) omop_recipe(
    concept_sets = list(cs = omop_concept_set(id, name = "cs")),
    variables = list(v = omop_variable(event = omop_event("measurement",
      omop_param("concept", type = "concept_set", default = "cs")), reducer = omop_reduce("count"))),
    outputs = omop_output("w", format = "wide"))
  a <- mk(2)
  b <- mk(1)
  target <- recipe_merge(recipe_bind(a, concept = "cs"), recipe_bind(b, concept = "cs"), prefix = "b")
  for (r in .cmp_roundtrips(recipe_merge(a, b, prefix = "b"))) {
    expect_identical(names(recipe_params(r)), "concept")
    .cmp_same(r, target)
    .cmp_same(recipe_bind(r, concept = "cs"), target)
  }
})

test_that("named inline concept sets in literals defaults and later bindings are prefixed", {
  mk <- function(source) omop_recipe(
    variables = list(v = omop_variable(event = source, reducer = omop_reduce("count"))),
    outputs = omop_output("w", format = "wide"))
  ev <- function(id) omop_event("measurement", omop_concept_set(id, name = "cs"), name = "ev")
  a <- mk(ev(2))
  b <- mk(ev(1))
  target <- recipe_merge(a, b, prefix = "b")
  plan <- recipe_to_plan(target)
  expect_true(all(c("cs", "b_cs") %in% names(plan$concept_sets)))
  expect_identical(plan$events$b_ev$concept_set, "b_cs")
  expect_error(recipe_merge(a, b), "disagree")
  for (default in list(NULL, ev(1))) {
    deferred <- mk(omop_param("source", type = "event", default = default))
    for (r in .cmp_roundtrips(recipe_merge(a, deferred, prefix = "b"))) {
      .cmp_same(recipe_bind(r, source = ev(1)), target)
      if (!is.null(default)) .cmp_same(r, target)
    }
  }
  # A concept-set object can itself be the deferred value.
  cs_b <- mk(omop_event("measurement", omop_param("concept", type = "concept_set"), name = "ev"))
  for (r in .cmp_roundtrips(recipe_merge(a, cs_b, prefix = "b"))) {
    .cmp_same(recipe_bind(r, concept = omop_concept_set(1, name = "cs")), target)
  }
})

.cmp_study <- function(washout) {
  pop <- omop_new_users(1503297, washout_days = washout, select = "all", by = "concept")
  pop$coverage <- omop_coverage("union", gap = 7L)
  # Independently supplied criteria must never be removed with generated ones.
  pop$filters <- c(pop$filters, list(omop_observed(before = 0L)))
  omop_study(pop, require = list(prior_observation = 45L, followup = 10L),
             baseline = list(age = omop_age()), outputs = "baseline")
}

.cmp_advanced <- function(washout) {
  rx <- omop_event("drug_exposure", 1503297)
  generated <- if (washout == 0L) list() else list(omop_observed(before = washout),
    omop_has_not(rx, window = omop_window(-washout, -1L)))
  omop_recipe(populations = list(omop_population("study", label = "New users",
    index = omop_index(omop_era(rx, by = "concept"), select = "all", end = omop_end("interval_end")),
    coverage = omop_coverage("union", gap = 7L),
    filters = c(generated, list(omop_observed(before = 0L), omop_observed(before = 45L),
                                omop_observed(after = 10L))))),
    variables = list(age = omop_age()),
    outputs = omop_output("baseline", rows = "study", format = "wide", grain = "episode"))
}

test_that("literal default explicit and staged washouts match the advanced expansion", {
  for (days in c(0L, 30L)) {
    expected <- .cmp_advanced(days)
    .cmp_same(.cmp_study(days), expected)
    defaulted <- .cmp_study(omop_param("washout", type = "integer", default = days))
    unbound <- .cmp_study(omop_param("washout", type = "integer"))
    for (r in .cmp_roundtrips(defaulted)) .cmp_same(r, expected)
    for (r in .cmp_roundtrips(unbound)) {
      bound <- recipe_bind(r, washout = days)
      .cmp_same(bound, expected)
      expect_identical(bound$populations$study$index, .cmp_study(days)$populations$study$index)
      expect_identical(bound$populations$study$coverage, omop_coverage("union", gap = 7L))
      for (rr in .cmp_roundtrips(bound)) .cmp_same(rr, expected)
    }
    expr <- omop_param("a", type = "integer") + omop_param("b", type = "integer")
    staged <- recipe_bind(.cmp_study(expr), a = days)
    expect_identical(names(recipe_params(staged)), "b")
    for (r in .cmp_roundtrips(staged)) .cmp_same(recipe_bind(r, b = 0L), expected)
  }
  expect_error(recipe_bind(.cmp_study(omop_param("w", type = "integer")), w = -1L), ">= 0")
  expect_error(omop_window(0L, -1L), "start must not be after end")
})

test_that("zero washout defaults compose with reference parameters and merge", {
  make <- function(w, cs) omop_study(omop_new_users(cs, washout_days = w),
    baseline = list(age = omop_age()), outputs = "baseline")
  w <- omop_param("w", type = "integer", default = 0L)
  cs <- omop_param("cs", type = "concept_set")
  a <- make(0L, omop_concept_set(2, name = "drug"))
  b <- make(w, cs)
  target <- recipe_merge(a, make(0L, omop_concept_set(1, name = "drug")), prefix = "b")
  for (r in .cmp_roundtrips(recipe_merge(a, b, prefix = "b"))) {
    .cmp_same(recipe_bind(r, cs = omop_concept_set(1, name = "drug")), target)
  }
})

test_that("string parameters are renamed only in source-reference occurrences", {
  p <- omop_param("name", type = "string", default = "ev")
  mk <- function(id) omop_recipe(events = list(omop_event("measurement", id, name = "ev")),
    variables = list(n = omop_variable(event = p, reducer = omop_reduce("count")),
      literal = omop_variable(event = omop_event("measurement", id,
        where = omop_where_tree(list(var = "value_as_concept_id", op = "==", value = p))),
        reducer = omop_reduce("count"))), outputs = omop_output("w", format = "wide"))
  a <- mk(2)
  b <- mk(1)
  expected <- recipe_merge(recipe_bind(a, name = "ev"), recipe_bind(b, name = "ev"), prefix = "b")
  for (r in .cmp_roundtrips(recipe_merge(a, b, prefix = "b"))) {
    .cmp_same(r, expected)
    .cmp_same(recipe_bind(r, name = "ev"), expected)
  }
})

test_that("nested unnamed concept exclusions retain deferred composition semantics", {
  mk <- function(id) omop_recipe(concept_sets = list(cs = omop_concept_set(id, name = "cs",
      exclude = omop_concept_set(3, exclude = omop_param("exclude", type = "concept_set", default = 4)))),
    variables = list(v = omop_variable(event = omop_event("measurement", "cs"),
      reducer = omop_reduce("count"))), outputs = omop_output("w", format = "wide"))
  a <- mk(2)
  b <- mk(1)
  expected <- recipe_merge(recipe_bind(a, exclude = c(4L, 5L)),
                            recipe_bind(b, exclude = c(4L, 5L)), prefix = "b")
  for (r in .cmp_roundtrips(recipe_merge(a, b, prefix = "b"))) {
    .cmp_same(recipe_bind(r, exclude = c(4L, 5L)), expected)
  }
  expect_error(omop_concept_set(1, exclude = omop_concept_set(2, name = "nested")), "give it no name")
})

test_that("resolved helper observability does not change shared base composition", {
  for (days in c(0L, 30L)) {
    simple <- omop_study(omop_new_users(1503297, washout_days = days), name = "base",
      baseline = list(age = omop_age()), outputs = "baseline")
    # The stored advanced population has exactly the same execution fields.
    advanced <- simple
    advanced$populations$base$simple <- NULL
    merged <- recipe_merge(advanced, simple, prefix = "b")
    .cmp_same(merged, recipe_merge(advanced, advanced, prefix = "b"))
    expect_identical(merged$populations$base$simple, simple$populations$base$simple)
    expect_output(print(merged), paste0("washout ", days, " days"))
    .cmp_same(recipe_merge(simple, advanced, prefix = "b"),
              recipe_merge(advanced, advanced, prefix = "b"))
  }
})
