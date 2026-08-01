test_that("date ranges compile once as a safe table-aware BETWEEN leaf", {
  r <- omop_recipe(
    variables = omop_variable(
      name = "events", table = "condition_occurrence", concept_id = 201820),
    filters = omop_filter_date_range("2020-01-01", "2020-01-31"),
    outputs = omop_output(name = "events", type = "long"))

  out <- recipe_to_plan(r)$outputs$events
  expect_null(out[["filter"]])
  expect_equal(out$filters$custom, list(
    var = "condition_start_date", op = "between",
    value = c("2020-01-01", "2020-01-31")))
  expect_false(is.null(out$filters$concept_set))
})

test_that("mixed-level OR and unsupported NOT groups fail closed", {
  mixed <- omop_filter_group(
    omop_filter_sex("F"),
    omop_filter_date_range("2020-01-01", "2020-03-01"),
    operator = "OR")
  r <- omop_recipe(
    variables = omop_variable(name = "event", table = "measurement"),
    filters = mixed,
    outputs = omop_output(name = "event", type = "long"))
  expect_error(recipe_to_plan(r), "OR group.*mixes filter levels")

  negated <- omop_filter_group(omop_filter_sex("F"), operator = "AND")
  negated$operator <- "NOT"
  r2 <- omop_recipe(
    variables = omop_variable_age(),
    filters = negated,
    outputs = omop_output(name = "age", type = "wide"))
  expect_error(recipe_to_plan(r2), "NOT groups are rejected")
})

test_that("tampered or imported filters are revalidated recursively", {
  invalid <- omop_filter_sex("F")
  invalid$type <- "min_count"
  nested <- omop_filter_group(invalid, operator = "AND")
  recipe <- omop_recipe(
    variables = omop_variable_age(),
    filters = nested,
    outputs = omop_output(name = "age", type = "wide"))
  expect_error(recipe_to_plan(recipe),
               "Filter in recipe filters is not executable.*no executable")

  wrong_level <- omop_filter_date_range("2020-01-01", "2020-03-01")
  wrong_level$level <- "population"
  recipe$filters <- list(wrong_level)
  expect_error(recipe_to_plan(recipe), "executable only at level 'row'")
})

test_that("generic custom filters are typed, row-only and fail closed", {
  custom <- omop_filter(
    type = "custom",
    params = list(var = "condition_type_concept_id", op = "in",
                  value = 32020L))
  expect_identical(custom$level, "row")
  expect_equal(.filter_to_leaf(custom), list(
    var = "condition_type_concept_id", op = "in", value = 32020L))

  expect_error(omop_filter(
    type = "custom", level = "population",
    params = list(var = "condition_type_concept_id", op = "in",
                  value = 32020L)),
    "executable only at level 'row'")
  expect_error(omop_filter(
    type = "custom",
    params = list(var = "value_as_number", op = ">", value = 5)),
    "ordered client-authored thresholds are not executable")
})

test_that("non-executable output labels and options never degrade silently", {
  expect_error(omop_output(type = "joined_long"),
               "no faithful executable mapping")
  expect_error(omop_output(type = "covariates_sparse"),
               "no faithful executable mapping")

  r <- omop_recipe(
    variables = omop_variable(
      name = "count", table = "condition_occurrence",
      concept_id = 201820, format = "count"),
    outputs = omop_output(
      name = "features", type = "features",
      options = list(sparse = TRUE)))
  expect_error(recipe_to_plan(r), "unsupported option.*sparse")
})

test_that("long output preserves population, temporal, calendar and row filters", {
  r <- omop_recipe(
    populations = omop_population(
      id = "adults", parent_id = "base",
      filters = omop_filter_age(18, 90)),
    variables = omop_variable(
      name = "lab_rows", table = "measurement", concept_id = 3004410,
      column = "value_as_number", format = "raw",
      time_window = list(start = -90, end = 0)),
    filters = omop_filter_date_range("2020-01-01", "2020-03-01"),
    outputs = omop_output(
      name = "labs", type = "long", population_id = "adults",
      options = list(
        temporal = list(index_window = list(start = -90, end = 0)),
        time_window = list(start_date = "2020-01-01"),
        date_handling = "relative")))

  plan <- recipe_to_plan(r)
  out <- plan$outputs$labs
  expect_equal(out$population_id, "adults")
  expect_equal(out$options, r$outputs$labs$options)
  expect_equal(out$temporal$index_window, list(start = -90L, end = 0L))
  expect_equal(out$filters$time_window$start_date, "2020-01-01")
  expect_equal(out$filters$custom$op, "between")
  expect_equal(out$date_handling, "relative")
  expect_null(out[["filter"]])
})

test_that("wide event rows require explicit reduction while person aliases work", {
  raw_event <- omop_recipe(
    variables = omop_variable(
      name = "dx", table = "condition_occurrence",
      column = "condition_concept_id"),
    outputs = omop_output(name = "wide", type = "wide"))
  expect_error(recipe_to_plan(raw_event),
               "requires an explicit per-variable reduction")

  reduced <- omop_recipe(
    variables = omop_variable(
      name = "dx_count", table = "condition_occurrence",
      concept_id = 201820, format = "count"),
    outputs = omop_output(name = "wide", type = "wide"))
  expect_equal(
    recipe_to_plan(reduced)$outputs$wide$representation$features$dx_count$type,
    "count")

  person <- omop_recipe(
    variables = omop_variable(
      name = "birth_year", table = "person", column = "year_of_birth"),
    outputs = omop_output(name = "wide", type = "wide"))
  expect_equal(
    recipe_to_plan(person)$outputs$wide$tables$person$birth_year,
    "year_of_birth")
})

test_that("multi-table long splits and never creates a joined event frame", {
  r <- omop_recipe(
    variables = list(
      omop_variable(
        name = "lab_value", table = "measurement",
        concept_id = 3004410, column = "value_as_number"),
      omop_variable(
        name = "condition_code", table = "condition_occurrence",
        concept_id = 201820, column = "condition_concept_id")),
    outputs = omop_output(name = "events", type = "long"))

  plan <- recipe_to_plan(r)
  expect_setequal(names(plan$outputs),
                  c("events_measurement", "events_condition_occurrence"))
  expect_equal(plan$outputs$events_measurement$columns, "value_as_number")
  expect_equal(plan$outputs$events_condition_occurrence$columns,
               "condition_concept_id")
  expect_true(all(vapply(plan$outputs, function(x) {
    identical(x$type, "event_level")
  }, logical(1))))
})

test_that("same-table long variables with different scopes split by variable", {
  a <- omop_variable(
    name = "early", table = "measurement", concept_id = 1,
    time_window = list(start = -365, end = -31),
    filters = omop_filter_date_range("2020-01-01", "2020-03-01"))
  b <- omop_variable(
    name = "late", table = "measurement", concept_id = 2,
    time_window = list(start = -30, end = 0),
    filters = omop_filter_date_range("2021-01-01", "2021-03-01"))
  plan <- recipe_to_plan(omop_recipe(
    variables = list(a, b),
    outputs = omop_output(name = "events", type = "long")))

  expect_setequal(names(plan$outputs),
                  c("events_measurement_early", "events_measurement_late"))
  expect_equal(
    plan$outputs$events_measurement_early$filters$custom$value,
    c("2020-01-01", "2020-03-01"))
  expect_equal(
    plan$outputs$events_measurement_late$temporal$index_window,
    list(start = -30L, end = 0L))
})

test_that("feature windows compile at episode grain with per-spec scopes", {
  common <- omop_recipe(
    variables = list(
      omop_variable(name = "a", table = "measurement", concept_id = 1,
                    format = "count", time_window = list(start = -90, end = 0)),
      omop_variable(name = "b", table = "measurement", concept_id = 2,
                    format = "binary", time_window = list(start = -90, end = 0))),
    outputs = omop_output(name = "features", type = "features"))
  expect_error(recipe_to_plan(common),
               "grain='episode'")

  common$outputs$features$options <- list(grain = "episode")
  plan <- recipe_to_plan(common)
  output <- plan$outputs$features
  expect_equal(output$representation$grain, "episode")
  expect_equal(output$temporal$index_window,
               list(start = -90L, end = 0L))
  expect_equal(output$representation$features$a$time_window,
               list(start = -90L, end = 0L))
  expect_equal(output$representation$features$b$time_window,
               list(start = -90L, end = 0L))

  different <- common
  different$variables$a$time_window <- list(start = -365L, end = -31L)
  different$variables$b$time_window <- list(start = -30L, end = 0L)
  different_plan <- recipe_to_plan(different)$outputs$features
  expect_equal(different_plan$temporal$index_window,
               list(start = -365L, end = 0L))
  expect_equal(different_plan$representation$features$a$time_window,
               list(start = -365L, end = -31L))
  expect_equal(different_plan$representation$features$b$time_window,
               list(start = -30L, end = 0L))

  mixed <- common
  mixed$variables$b$time_window <- NULL
  expect_error(recipe_to_plan(mixed), "mixes variables with and without")
})

test_that("temporal_covariates maps to the executable plan helper per table", {
  r <- omop_recipe(
    variables = list(
      omop_variable(name = "dx", table = "condition_occurrence",
                    concept_id = 201820, format = "binary"),
      omop_variable(name = "drug", table = "drug_exposure",
                    concept_id = 1124300, format = "count")),
    outputs = omop_output(
      name = "tc", type = "temporal_covariates",
      options = list(bin_width = 14L, window_start = -180L,
                     window_end = 0L)))

  plan <- recipe_to_plan(r)
  expect_setequal(names(plan$outputs),
                  c("tc_condition_occurrence", "tc_drug_exposure"))
  expect_true(all(vapply(plan$outputs, function(x) {
      identical(x$type, "temporal_covariates") &&
      identical(x$bin_width, 14L) &&
      identical(x$population_id, "base")
  }, logical(1))))
  expect_equal(plan$outputs$tc_condition_occurrence$analyses, "binary")
  expect_equal(plan$outputs$tc_drug_exposure$analyses, "count")
})

test_that("every feature format maps explicitly and unknown formats stop", {
  expected <- c(
    binary = "boolean", count = "count", first_value = "first_value",
    last_value = "latest_value", mean = "mean_value", min = "min_value",
    max = "max_value", time_since = "time_since",
    drug_duration = "drug_duration", sum = "sum_value",
    n_distinct = "n_distinct", sd = "sd_value", cv = "cv_value",
    slope = "slope_value", abnormal_high = "abnormal_high",
    abnormal_low = "abnormal_low", gap_max = "gap_max_days",
    gap_mean = "gap_mean_days", duration_sum = "duration_sum")

  for (fmt in names(expected)) {
    args <- list(
      name = paste0("v_", fmt), table = "measurement",
      concept_id = 3004410, format = fmt,
      value_source = if (fmt %in% c("first_value", "last_value", "mean",
                                    "min", "max", "sum", "sd", "cv",
                                    "slope")) "value_as_number" else NULL)
    if (identical(fmt, "time_since")) {
      args$reference_date <- "2024-01-31"
      args$unit <- "month"
    }
    v <- do.call(omop_variable, args)
    spec <- dsOMOPClient:::.build_feature_specs(list(v))[[1]]
    expect_equal(spec$type, unname(expected[[fmt]]), info = fmt)
    if (identical(fmt, "time_since")) {
      expect_equal(spec$reference_date, "2024-01-31")
      expect_equal(spec$unit, "month")
    }
  }

  bad <- omop_variable(name = "bad", table = "measurement",
                       concept_id = 3004410, format = "count")
  bad$format <- "unknown"
  expect_error(dsOMOPClient:::.build_feature_specs(list(bad)),
               "no executable feature mapping")

  time_since <- bad
  time_since$format <- "time_since"
  expect_error(dsOMOPClient:::.build_feature_specs(list(time_since)),
               "derived\\$reference_date.*derived\\$unit")
})

test_that("features outputs route person-derived variables through derived columns", {
  recipe <- omop_recipe(
    variables = list(omop_variable_sex(), omop_variable_age()),
    outputs = omop_output("derived_features", type = "features")
  )

  plan <- recipe_to_plan(recipe)
  output <- plan$outputs$derived_features

  expect_equal(output$type, "person_level")
  expect_equal(output$representation, "features")
  expect_equal(
    unname(vapply(output$derived_columns, `[[`, character(1), "kind")),
    c("sex_mf", "age")
  )
})

test_that("feature specs keep independent concept scopes without a global filter", {
  all_concepts <- omop_variable(
    name = "diversity", table = "measurement", format = "n_distinct")
  unit_scoped <- omop_variable(
    name = "in_mg_dl", table = "measurement", concept_id = 8840,
    format = "count", concept_col = "unit_concept_id")
  expanded <- omop_variable(
    name = "expanded_dx", table = "measurement", concept_id = 3004410,
    format = "binary", expand = TRUE)
  exact <- omop_variable(
    name = "exact_dx", table = "measurement", concept_id = 3004410,
    format = "binary")
  plan <- recipe_to_plan(omop_recipe(
    variables = list(all_concepts, unit_scoped, expanded, exact),
    outputs = omop_output(name = "features", type = "features")))

  out <- plan$outputs$features
  expect_null(out$concept_set)
  expect_null(out$filters$concept_set)
  expect_length(out$representation$features$diversity$concept_set, 0L)
  expect_equal(out$representation$features$in_mg_dl$concept_col,
               "unit_concept_id")
  expect_equal(out$representation$features$expanded_dx$concept_set,
               list(concepts = 3004410L, include_descendants = TRUE))
  expect_identical(out$representation$features$exact_dx$concept_set,
                   3004410L)
})

test_that("time_since propagates fixed-reference semantics and schema", {
  recency <- omop_variable(
    name = "mi_recency", table = "condition_occurrence", concept_id = 316866,
    format = "time_since", reference_date = "2024-01-31", unit = "month")
  r <- omop_recipe(
    variables = recency,
    outputs = omop_output(name = "features", type = "features"))
  spec <- recipe_to_plan(r)$outputs$features$representation$features$mi_recency
  expect_equal(spec$reference_date, "2024-01-31")
  expect_equal(spec$unit, "month")

  row <- recipe_preview_schema(r)$features
  row <- row[row$column == "mi_recency", , drop = FALSE]
  expect_equal(row$r_type, "integer")

  restored <- recipe_import_json(recipe_export_json(r))
  expect_equal(restored$variables$mi_recency$derived, recency$derived)

  block_recipe <- omop_recipe(
    blocks = omop_variable_block(
      table = "condition_occurrence", concept_ids = c(1L, 2L),
      format = "time_since", reference_date = "2024-01-31", unit = "month"),
    outputs = omop_output(name = "block_features", type = "features"))
  block_out <- recipe_to_plan(block_recipe)$outputs$block_features
  block_specs <- block_out$representation$features
  expect_true(all(vapply(block_specs, function(x) {
    identical(x$reference_date, "2024-01-31") && identical(x$unit, "month")
  }, logical(1))))
})

test_that("long splits scoped/unscoped and mixed expansion semantics", {
  all_rows <- omop_variable(name = "all_rows", table = "measurement")
  expanded <- omop_variable(name = "expanded", table = "measurement",
                            concept_id = 3004410, expand = TRUE)
  plan <- recipe_to_plan(omop_recipe(
    variables = list(all_rows, expanded),
    outputs = omop_output(name = "events", type = "long")))

  expect_setequal(names(plan$outputs),
                  c("events_measurement_all_rows",
                    "events_measurement_expanded"))
  expect_null(plan$outputs$events_measurement_all_rows$concept_set)
  expect_true(is.list(
    plan$outputs$events_measurement_expanded$concept_set))
})

test_that("survival and intervals reject variable semantics they cannot carry", {
  visit_scoped <- omop_variable(
    name = "outcome", table = "condition_occurrence", concept_id = 201820,
    visit_filter = list(concept_ids = 9201L))
  survival <- omop_recipe(
    variables = visit_scoped,
    outputs = omop_output(name = "tte", type = "survival"))
  expect_error(recipe_to_plan(survival), "visit/concept-column overrides")

  unscoped <- omop_variable(name = "all", table = "drug_exposure")
  scoped <- omop_variable(name = "drug", table = "drug_exposure",
                          concept_id = 1124300)
  intervals <- omop_recipe(
    variables = list(unscoped, scoped),
    outputs = omop_output(name = "iv", type = "intervals"))
  expect_error(recipe_to_plan(intervals),
               "mixes concept-scoped and unscoped")
})

test_that("temporal covariates reject undeclared Cartesian products", {
  binary <- omop_variable(name = "presence", table = "measurement",
                          concept_id = 1, format = "binary")
  count <- omop_variable(name = "count", table = "measurement",
                         concept_id = 2, format = "count")
  mixed <- omop_recipe(
    variables = list(binary, count),
    outputs = omop_output(name = "tc", type = "temporal_covariates"))
  expect_error(recipe_to_plan(mixed), "Cartesian product")

  unscoped <- binary
  unscoped$concept_id <- NULL
  expect_error(recipe_to_plan(omop_recipe(
    variables = unscoped,
    outputs = omop_output(name = "tc", type = "temporal_covariates"))),
    "every variable to be concept-scoped")

  complete <- omop_recipe(
    variables = list(
      binary,
      omop_variable(name = "one_count", table = "measurement",
                    concept_id = 1, format = "count"),
      omop_variable(name = "two_presence", table = "measurement",
                    concept_id = 2, format = "binary"),
      count),
    outputs = omop_output(name = "tc", type = "temporal_covariates"))
  expect_equal(recipe_to_plan(complete)$outputs$tc$analyses,
               c("binary", "count"))
})

test_that("temporal preview includes the row-to-person reference", {
  r <- omop_recipe(
    variables = omop_variable(name = "dx", table = "condition_occurrence",
                              concept_id = 201820, format = "binary"),
    outputs = omop_output(name = "tc", type = "temporal_covariates"))
  schema <- recipe_preview_schema(r)$tc
  expect_true(all(c("personRef.rowId", "personRef.person_id") %in%
                    schema$column))
})

test_that("split output symbols resolve against the longest recipe parent", {
  vars <- list(
    omop_variable(name = "lab", table = "measurement", concept_id = 1),
    omop_variable(name = "dx", table = "condition_occurrence", concept_id = 2))
  r <- omop_recipe(
    variables = vars,
    outputs = list(
      omop_output(name = "labs", type = "long", result_symbol = "D_short"),
      omop_output(name = "labs_recent", type = "long",
                  result_symbol = "D_long")))
  seen <- NULL
  testthat::local_mocked_bindings(
    ds.omop.plan.execute = function(plan, out, symbol = "omop", conns = NULL,
                                    output_mode = "memory") {
      seen <<- out
      invisible(out)
    },
    .package = "dsOMOPClient"
  )
  recipe_execute(r)
  expect_equal(seen[["labs_recent_measurement"]], "D_long_measurement")
  expect_equal(seen[["labs_measurement"]], "D_short_measurement")
})

test_that("preview reports materialized long columns and split frames, not aliases", {
  r <- omop_recipe(
    variables = omop_variable(
      name = "friendly_alias", table = "measurement",
      concept_id = 3004410, column = "value_as_number"),
    outputs = omop_output(name = "events", type = "long"))
  schema <- recipe_preview_schema(r)$events
  expect_true("value_as_number" %in% schema$column)
  expect_false("friendly_alias" %in% schema$column)
  expect_equal(attr(schema, "plan_outputs"), "events")
})
