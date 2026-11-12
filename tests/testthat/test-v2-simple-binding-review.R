# WS9H R3/R4: literal helper expansion is the oracle for every resolution path.
.sbr_roundtrips <- function(recipe) {
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(recipe)), envir = env)
  forms <- list(recipe, env$recipe, recipe_import_json(recipe_export_json(recipe)))
  if (requireNamespace("yaml", quietly = TRUE)) {
    forms <- c(forms, list(recipe_import_yaml(recipe_export_yaml(recipe))))
  }
  forms
}

.sbr_same <- function(actual, literal) {
  actual <- recipe_to_plan(actual)
  literal <- recipe_to_plan(literal)
  expect_identical(actual, literal)
  expect_identical(dsOMOPClient:::.plan_v2_digest(actual)$sha256,
                   dsOMOPClient:::.plan_v2_digest(literal)$sha256)
}

.sbr_value <- function(unit, before = 30L) {
  omop_study(list(cohort_definition_id = 1L), outputs = "baseline",
    baseline = list(lab = omop_mean_value("measurement", 3004410L,
                                          unit = unit, before = before)))
}

.sbr_count <- function(distinct, before = 30L) {
  omop_study(list(cohort_definition_id = 1L), outputs = "baseline",
    baseline = list(n = omop_count("measurement", 3004410L,
                                   distinct = distinct, before = before)))
}

.sbr_users <- function(drug, washout = 30L) {
  p <- omop_new_users(drug, washout_days = washout)
  p$label <- "Edited users"
  p$index$offset <- 2L
  p$index$source$gap <- 17L
  p$filters <- c(p$filters, list(omop_age_is(min = 40L)))
  omop_study(p, name = "users", outputs = "baseline",
    require = list(prior_observation = 20L), baseline = list(age = omop_age()))
}

test_that("numeric unit placeholders use the literal restriction after every resolution path", {
  for (unit in list(8554L, c(8554L, 8753L))) {
    type <- if (length(unit) == 1L) "integer" else "concept_set"
    pending <- .sbr_value(omop_param("unit", type, default = unit))
    literal <- .sbr_value(unit)
    for (r in .sbr_roundtrips(pending)) {
      .sbr_same(r, literal)
      .sbr_same(recipe_bind(r, unit = unit), literal)
      event <- recipe_to_plan(r)$events
      expect_true(all(vapply(event, function(e) is.null(e$unit), logical(1))))
    }
    staged <- .sbr_value(omop_param("unit", type), omop_param("before", "integer"))
    for (first in c("unit", "before")) {
      half <- if (first == "unit") recipe_bind(staged, unit = unit) else
        recipe_bind(staged, before = 30L)
      for (r in .sbr_roundtrips(half)) {
        bound <- if (first == "unit") recipe_bind(r, before = 30L) else
          recipe_bind(r, unit = unit)
        .sbr_same(bound, literal)
      }
    }
  }
  for (unit in list(-1, 1.5)) {
    for (r in .sbr_roundtrips(.sbr_value(omop_param("unit", "number", default = unit)))) {
      expect_error(recipe_to_plan(r), "non-negative integer concept IDs")
      expect_error(recipe_bind(r, unit = unit), "non-negative integer concept IDs")
    }
  }
  pending <- .sbr_value(omop_param("unit", "integer"))
  literal <- .sbr_value(8554L)
  pending$variables$lab$event$where <- omop_where(value_as_number > 1)
  literal$variables$lab$event$where <- pending$variables$lab$event$where
  for (r in .sbr_roundtrips(pending)) .sbr_same(recipe_bind(r, unit = 8554L), literal)
  # Constructor canonicalization of a raw predicate is not a user edit.
  for (where in list(omop_where(value_as_number > 1),
                     list(var = "value_as_number", op = ">", value = 1))) {
    make_event <- function(unit) dsOMOPClient:::.simple_event("measurement", 3004410L,
      list(where = where), "simple helper", unit = unit)
    expect_error(make_event(8554L), "numeric unit or a where predicate")
    event <- make_event(omop_param("unit", "integer", default = 8554L))
    expect_identical(event$simple$where, event$where)
    original_conflict <- omop_recipe(variables = list(lab = omop_variable(
      event = event, reducer = omop_reduce("mean"))), outputs = omop_output("wide"))
    for (r in .sbr_roundtrips(original_conflict)) {
      expect_error(recipe_to_plan(r), "numeric unit or a where predicate")
      expect_error(recipe_bind(r, unit = 8554L), "numeric unit or a where predicate")
    }
  }
})

test_that("row count placeholders omit distinct exactly like the simple literal helper", {
  for (distinct in c("row", "day", "visit", "concept")) {
    pending <- .sbr_count(omop_param("distinct", "string", default = distinct))
    literal <- .sbr_count(distinct)
    for (r in .sbr_roundtrips(pending)) {
      .sbr_same(r, literal)
      .sbr_same(recipe_bind(r, distinct = distinct), literal)
    }
    staged <- .sbr_count(omop_param("distinct", "string"), omop_param("before", "integer"))
    for (half in list(recipe_bind(staged, before = 30L),
                     recipe_bind(staged, distinct = distinct))) {
      for (r in .sbr_roundtrips(half)) {
        bound <- if ("distinct" %in% names(recipe_params(r))) {
          recipe_bind(r, distinct = distinct)
        } else recipe_bind(r, before = 30L)
        .sbr_same(bound, literal)
      }
    }
  }
  for (r in .sbr_roundtrips(.sbr_count(omop_param("distinct", "string", default = "invalid")))) {
    expect_error(recipe_to_plan(r), "distinct must be one of")
    expect_error(recipe_bind(r, distinct = "invalid"), "distinct must be one of")
  }
})

test_that("new-user event placeholders retain options, current edits and independent requirements", {
  drug <- omop_event("drug_exposure", omop_concept_set(1503297L, descendants = TRUE),
    where = omop_where(quantity > 1), visit = "index", end = list(fallback = "days_supply"),
    name = "rx", on_missing_table = "unavailable")
  for (washout in c(0L, 30L)) {
    pending <- .sbr_users(omop_param("drug", "event", default = drug), washout)
    literal <- .sbr_users(drug, washout)
    for (r in .sbr_roundtrips(pending)) {
      .sbr_same(r, literal)
      .sbr_same(recipe_bind(r, drug = drug), literal)
    }
    staged <- .sbr_users(omop_param("drug", "event"), omop_param("washout", "integer"))
    for (first in c("drug", "washout")) {
      half <- if (first == "drug") recipe_bind(staged, drug = drug) else
        recipe_bind(staged, washout = washout)
      for (r in .sbr_roundtrips(half)) {
        bound <- if (first == "drug") recipe_bind(r, washout = washout) else
          recipe_bind(r, drug = drug)
        .sbr_same(bound, literal)
      }
    }
  }
  for (invalid in list(omop_event("measurement", 1L), omop_era(drug))) {
    for (r in .sbr_roundtrips(.sbr_users(omop_param("drug", "event", default = invalid)))) {
      expect_error(recipe_to_plan(r), "drug_exposure|eras are built")
      expect_error(recipe_bind(r, drug = invalid), "drug_exposure|eras are built")
    }
  }
})

test_that("event-typed new-user names resolve the current drug event registry", {
  drug <- omop_event("drug_exposure", 1503297L, name = "rx",
                     where = omop_where(quantity > 1))
  make <- function(source) {
    r <- .sbr_users(source, omop_param("washout", "integer", default = 0L))
    r$events <- list(rx = drug)
    # A concept set with the same name must never capture the event reference.
    r$concept_sets <- list(rx = omop_concept_set(999L, name = "rx"))
    r
  }
  pending <- make(omop_param("drug", "event", default = "rx"))
  literal <- make(drug)
  nest <- function(r) recipe_merge(omop_recipe(),
    recipe_merge(omop_recipe(), r, prefix = "inner"), prefix = "outer")
  for (pair in list(list(pending, literal), list(nest(pending), nest(literal)))) {
    for (r in .sbr_roundtrips(pair[[1L]])) {
      .sbr_same(r, pair[[2L]])
      .sbr_same(recipe_bind(recipe_bind(r, washout = 0L), drug = "rx"), pair[[2L]])
      .sbr_same(recipe_bind(recipe_bind(r, drug = "rx"), washout = 0L), pair[[2L]])
    }
  }
  for (source in list(omop_event("measurement", 1L, name = "rx"),
                      omop_era(drug, name = "rx"))) {
    r <- pending
    r$events <- if (inherits(source, "omop_event")) list(rx = source) else list()
    r$intervals <- if (inherits(source, "omop_interval")) list(rx = source) else list()
    expect_error(recipe_to_plan(r), "drug_exposure|eras are built")
  }
  expect_error(recipe_bind(pending, drug = "absent"), "unknown drug event")
})

test_that("deferred simple choices retain namespace maps and edits through nested merges", {
  drug <- omop_event("drug_exposure", omop_concept_set(1503297L, name = "drug_codes"),
                     name = "rx", where = omop_where(quantity > 1))
  make <- function(drug, unit, distinct, washout) {
    r <- .sbr_users(drug, washout)
    r$variables$lab <- omop_mean_value("measurement", 3004410L, unit = unit,
                                      before = omop_param("before", "integer", default = 30L))
    r$variables$n <- omop_count("measurement", distinct = distinct)
    r
  }
  pending <- make(omop_param("drug", "event", default = drug),
    omop_param("unit", "integer", default = 8554L),
    omop_param("distinct", "string", default = "row"),
    omop_param("washout", "integer", default = 0L))
  literal <- make(drug, 8554L, "row", 0L)
  nest <- function(r) recipe_merge(omop_recipe(),
    recipe_merge(omop_recipe(), r, prefix = "inner"), prefix = "outer")
  pending <- nest(pending)
  literal <- nest(literal)
  for (r in .sbr_roundtrips(pending)) .sbr_same(r, literal)
  half <- recipe_bind(pending, unit = 8554L, washout = 0L, before = 30L)
  # Edits apply to the current graph; helper expansion cannot replay snapshots.
  edit <- function(r) {
    r$variables$outer_inner_lab$event$value <- "range_low"
    r$variables$outer_inner_n$reducer$window <- omop_window(-7L, 0L)
    r$populations$outer_inner_users$filters <- c(
      r$populations$outer_inner_users$filters, list(omop_observed(after = 10L)))
    r
  }
  literal <- edit(literal)
  for (r in .sbr_roundtrips(edit(half))) {
    .sbr_same(recipe_bind(r, drug = drug, distinct = "row"), literal)
  }
})

test_that("finite parameter arithmetic is independent of integer literal storage", {
  for (multiplier in list(60000L, 60000)) {
    n <- omop_param("n", "integer", default = 60000L)
    expr <- (n * multiplier) / 60000L
    pending <- .sbr_users(1503297L, expr)
    literal <- .sbr_users(1503297L, 60000L)
    for (r in .sbr_roundtrips(pending)) {
      .sbr_same(r, literal)
      .sbr_same(recipe_bind(r, n = 60000L), literal)
    }
    denominator <- omop_param("denominator", "integer")
    staged <- .sbr_users(1503297L, (n * multiplier) / denominator)
    for (r in .sbr_roundtrips(recipe_bind(staged, n = 60000L))) {
      .sbr_same(recipe_bind(r, denominator = 60000L), literal)
    }
    for (r in .sbr_roundtrips(recipe_bind(staged, denominator = 60000L))) {
      .sbr_same(recipe_bind(r, n = 60000L), literal)
    }
  }
  n <- omop_param("n", "integer", default = 60000L)
  # Addition/subtraction also retain finite intermediates outside integer storage.
  for (expr in list((n + 2147483647L) - 2147483647L,
                    (n - (-2147483647L)) + (-2147483647L))) {
    for (r in .sbr_roundtrips(.sbr_users(1503297L, expr))) {
      .sbr_same(r, .sbr_users(1503297L, 60000L))
    }
  }
  expect_error(recipe_to_plan(.sbr_users(1503297L, n / 0L)), "no finite value")
  expect_error(recipe_bind(.sbr_users(1503297L, n * 1e308), n = 60000L), "no finite value")
  expect_error(recipe_to_plan(.sbr_users(1503297L, n * 60000L)), "exact integer")
  expect_error(recipe_to_plan(.sbr_users(1503297L, n / 7L)), "exact integer")
})


test_that("event helper serialization retains mixed numeric predicate literal storage", {
  drug <- omop_event("drug_exposure", 1503297L, name = "rx",
    where = omop_where(quantity %in% c(1L, 2.5, 3)))
  pending <- .sbr_users(omop_param("drug", "event", default = drug))
  literal <- .sbr_users(drug)
  for (r in .sbr_roundtrips(pending)) {
    .sbr_same(r, literal)
    .sbr_same(recipe_bind(r, drug = drug), literal)
  }
  # Storage annotations cannot truncate or bypass ordinary predicate validation.
  plain <- dsOMOPClient:::.v2_plain_value(omop_where(quantity > 1.5))
  plain$omop_numeric_storage[[1L]]$type <- "integer"
  expect_error(dsOMOPClient:::.v2_restore(plain), "Invalid predicate numeric storage")
  plain$omop_numeric_storage[[1L]]$type <- "double"
  plain$omop_numeric_storage[[1L]]$path <- c(500L)
  expect_error(dsOMOPClient:::.v2_restore(plain), "Invalid predicate numeric storage")
})
