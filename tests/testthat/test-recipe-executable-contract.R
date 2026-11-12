test_that("date ranges compile once as a safe table-aware BETWEEN leaf", {
  r <- omop_recipe(
    variables = omop_variable(
      name = "events", table = "condition_occurrence", concept_id = 201820),
    filters = omop_filter_date_range("2020-01-01", "2020-01-31"),
    outputs = omop_output(name = "events", type = "long"))

  out <- recipe_to_plan(r, version = 1)$outputs$events
  expect_null(out[["filter"]])
  expect_equal(out$filters$custom, list(
    var = "condition_start_date", op = "between",
    value = c("2020-01-01", "2020-01-31")))
  expect_false(is.null(out$filters$concept_set))
})

test_that("mixed-level OR fails closed; NOT groups are newly permitted", {
  mixed <- omop_filter_group(
    omop_filter_sex("F"),
    omop_filter_date_range("2020-01-01", "2020-03-01"),
    operator = "OR")
  r <- omop_recipe(
    variables = omop_variable(name = "event", table = "measurement"),
    filters = mixed,
    outputs = omop_output(name = "event", type = "long"))
  expect_error(recipe_to_plan(r, version = 1), "OR group.*mixes filter levels")
  wide <- omop_recipe(variables = omop_variable_sex(), filters = mixed,
                      outputs = omop_output(name = "w", type = "wide"))
  expect_error(recipe_to_plan(wide, version = 1), "OR group.*mixes filter levels")
  expect_error(recipe_to_plan(wide), "row")

  # 2.10.3 (design 6.6): a NOT group is omop_not() of the language-2 plan; it
  # has no v1 rendering, so it runs only on 2.10.3 servers.
  negated <- omop_filter_group(omop_filter_sex("F"), operator = "NOT")
  expect_identical(negated$label, "NOT (Sex = F)")
  r2 <- omop_recipe(
    variables = omop_variable_age(),
    filters = negated,
    outputs = omop_output(name = "age", type = "wide"))
  plan <- recipe_to_plan(r2)
  expect_identical(plan$populations$base$where,
                   list(not = list(person = list(var = "gender_concept_id", op = "==",
                                                 value = 8532L))))
  e <- tryCatch(recipe_to_plan(r2, version = 1), error = function(e) e)
  expect_s3_class(e, "dsomop_v1_newly_permitted")
  expect_match(conditionMessage(e), "no v1 rendering")
  expect_s3_class(dsOMOPClient:::.recipe_plan_auto(r2), "omop_plan_v2")
  # a row-level NOT group negates its rows
  rows <- omop_recipe(variables = list(n = omop_variable(
    name = "n", table = "condition_occurrence", concept_id = 201820, format = "count",
    filters = list(omop_filter_group(omop_filter_date_range("2020-01-01", "2020-12-31"),
                                     operator = "NOT")))),
    outputs = omop_output(name = "w", type = "wide"))
  ev <- recipe_to_plan(rows)$events[[1]]
  expect_identical(names(ev$where), "not")
})

test_that("tampered or imported filters are revalidated recursively", {
  invalid <- omop_filter_sex("F")
  invalid$type <- "min_count"
  nested <- omop_filter_group(invalid, operator = "AND")
  recipe <- omop_recipe(
    variables = omop_variable_age(),
    filters = nested,
    outputs = omop_output(name = "age", type = "wide"))
  expect_error(recipe_to_plan(recipe, version = 1),
               "Filter in recipe filters is not executable.*no executable")

  wrong_level <- omop_filter_date_range("2020-01-01", "2020-03-01")
  wrong_level$level <- "population"
  recipe$filters <- list(wrong_level)
  expect_error(recipe_to_plan(recipe, version = 1), "executable only at level 'row'")
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
  # 2.10.3 (F-23): ordered comparisons are newly permitted (the server's
  # final publication rule is the only release criterion); malformed operands
  # still fail closed.
  exact <- omop_filter(type = "custom",
    params = list(var = "value_as_number", op = ">", value = 5))
  expect_identical(.filter_to_leaf(exact),
                   list(var = "value_as_number", op = ">", value = 5))
  expect_error(omop_filter(type = "custom",
    params = list(var = "value_as_number", op = ">", value = c(1, 5))),
    "one finite numeric")
  # legacy operator spellings lower to the language-2 operators
  gte <- omop_recipe(variables = list(n = omop_variable(
    name = "n", table = "measurement", concept_id = 3004410, format = "count",
    filters = list(omop_filter(type = "custom", level = "row",
                               params = list(var = "value_as_number", op = "gte",
                                             value = 5))))),
    outputs = omop_output(name = "w", type = "wide"))
  ev <- recipe_to_plan(gte)$events[[1]]
  expect_identical(ev$where, list(var = "value_as_number", op = ">=", value = 5))
})

test_that("non-executable output labels and options never degrade silently", {
  expect_error(omop_output(type = "joined_long"),
               "no faithful executable mapping")
  # 2.10.3 (B-22): the covariates_sparse label is the sparse format
  expect_identical(omop_output(type = "covariates_sparse")$type, "covariates_sparse")

  r <- omop_recipe(
    variables = omop_variable(
      name = "count", table = "condition_occurrence",
      concept_id = 201820, format = "count"),
    outputs = omop_output(
      name = "features", type = "features",
      options = list(sparse = TRUE)))
  expect_error(recipe_to_plan(r, version = 1), "unsupported option.*sparse")
  expect_error(recipe_to_plan(r), "sparse")
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

  plan <- recipe_to_plan(r, version = 1)
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
  expect_error(recipe_to_plan(raw_event, version = 1),
               "requires an explicit per-variable reduction")

  reduced <- omop_recipe(
    variables = omop_variable(
      name = "dx_count", table = "condition_occurrence",
      concept_id = 201820, format = "count"),
    outputs = omop_output(name = "wide", type = "wide"))
  expect_equal(
    recipe_to_plan(reduced, version = 1)$outputs$wide$representation$features$dx_count$type,
    "count")

  person <- omop_recipe(
    variables = omop_variable(
      name = "birth_year", table = "person", column = "year_of_birth"),
    outputs = omop_output(name = "wide", type = "wide"))
  expect_equal(
    recipe_to_plan(person, version = 1)$outputs$wide$tables$person$birth_year,
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

  plan <- recipe_to_plan(r, version = 1)
  expect_setequal(names(plan$outputs),
                  c("events_measurement", "events_condition_occurrence"))
  expect_equal(plan$outputs$events_measurement$columns, "value_as_number")
  expect_equal(plan$outputs$events_condition_occurrence$columns,
               "condition_concept_id")
  expect_true(all(vapply(plan$outputs, function(x) {
    identical(x$type, "event_level")
  }, logical(1))))
})

test_that("compiled multi-table output names cannot overwrite explicit outputs", {
  recipe <- omop_recipe(
    variables = list(
      omop_variable(
        name = "condition", table = "condition_occurrence",
        concept_id = 201820
      ),
      omop_variable(
        name = "measurement", table = "measurement",
        concept_id = 3004410
      )
    ),
    outputs = list(
      omop_output(
        name = "x", type = "long",
        variables = c("condition", "measurement")
      ),
      omop_output(
        name = "x_condition_occurrence", type = "long",
        variables = "measurement"
      )
    )
  )

  expect_error(recipe_to_plan(recipe, version = 1), "compiled output name collision")
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
    outputs = omop_output(name = "events", type = "long")), version = 1)

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
  expect_error(recipe_to_plan(common, version = 1),
               "grain='episode'")
  # A-14: newly permitted -- the v1 refusal keeps its message and carries the
  # class. Over this unindexed base the v2 rendering (episode grain) is refused
  # plainly (review B5); over an indexed population it compiles and execution
  # selects it.
  e <- tryCatch(recipe_to_plan(common, version = 1), error = function(e) e)
  expect_s3_class(e, "dsomop_v1_newly_permitted")
  expect_error(recipe_to_plan(common), "episode grain needs an indexed population")
  expect_error(dsOMOPClient:::.recipe_plan_auto(common), "cannot execute as a v1 plan either")
  index_p <- omop_population("p", index_event = omop_index_event(201820, "condition_occurrence"))
  indexed <- common
  indexed$populations$p <- index_p
  indexed$outputs$features$population_id <- "p"
  expect_identical(recipe_to_plan(indexed)$outputs$features$grain, "episode")
  expect_s3_class(dsOMOPClient:::.recipe_plan_auto(indexed), "omop_plan_v2")

  common$outputs$features$options <- list(grain = "episode")
  plan <- recipe_to_plan(common, version = 1)
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
  different_plan <- recipe_to_plan(different, version = 1)$outputs$features
  expect_equal(different_plan$temporal$index_window,
               list(start = -365L, end = 0L))
  expect_equal(different_plan$representation$features$a$time_window,
               list(start = -365L, end = -31L))
  expect_equal(different_plan$representation$features$b$time_window,
               list(start = -30L, end = 0L))

  mixed <- common
  mixed$variables$b$time_window <- NULL
  expect_error(recipe_to_plan(mixed, version = 1), "mixes variables with and without")
  # 2.10.3 (B-19): one roster, many sources and windows, over an indexed
  # population (episode grain needs one, review B5)
  e <- tryCatch(recipe_to_plan(mixed, version = 1), error = function(e) e)
  expect_s3_class(e, "dsomop_v1_newly_permitted")
  expect_error(recipe_to_plan(mixed), "episode grain needs an indexed population")
  mixed$populations$p <- index_p
  mixed$outputs$features$population_id <- "p"
  plan <- recipe_to_plan(mixed)
  expect_identical(plan$outputs$features$format, "wide")
  expect_identical(plan$outputs$features$grain, "episode")
  expect_identical(plan$variables$a$reducer$window$start, -90L)
  expect_null(plan$variables$b$reducer$window)
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

  plan <- recipe_to_plan(r, version = 1)
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

  plan <- recipe_to_plan(recipe, version = 1)
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
    outputs = omop_output(name = "features", type = "features")), version = 1)

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
  spec <- recipe_to_plan(r, version = 1)$outputs$features$representation$features$mi_recency
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
  block_out <- recipe_to_plan(block_recipe, version = 1)$outputs$block_features
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
    outputs = omop_output(name = "events", type = "long")), version = 1)

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
  expect_error(recipe_to_plan(survival, version = 1), "visit/concept-column overrides")

  unscoped <- omop_variable(name = "all", table = "drug_exposure")
  scoped <- omop_variable(name = "drug", table = "drug_exposure",
                          concept_id = 1124300)
  intervals <- omop_recipe(
    variables = list(unscoped, scoped),
    outputs = omop_output(name = "iv", type = "intervals"))
  expect_error(recipe_to_plan(intervals, version = 1),
               "mixes concept-scoped and unscoped")
  # 2.10.3 (design 2.2, 6.12): language-2 sources carry visit and concept
  # column; survival keeps the legacy TAR and censoring (cohort-end cap).
  for (r in list(survival, intervals)) {
    e <- tryCatch(recipe_to_plan(r, version = 1), error = function(e) e)
    expect_s3_class(e, "dsomop_v1_newly_permitted")
  }
  sp <- recipe_to_plan(survival)
  so <- sp$outputs$tte$options
  expect_identical(sp$events[[so$outcomes$outcome$event]]$visit, list(type = list(9201L)))
  expect_identical(so$censor, list("observation_end", "index_end"))
  expect_identical(so$tar$end, 730L)
  ip <- recipe_to_plan(intervals)
  srcs <- vapply(ip$outputs$iv$options$sources, function(x) x$event, "")
  expect_length(srcs, 2L)
  expect_null(ip$events[[srcs[[1]]]]$concept_set)
  expect_identical(ip$events[[srcs[[2]]]]$concept_set$concepts, list(1124300L))
})

test_that("temporal covariates reject undeclared Cartesian products", {
  binary <- omop_variable(name = "presence", table = "measurement",
                          concept_id = 1, format = "binary")
  count <- omop_variable(name = "count", table = "measurement",
                         concept_id = 2, format = "count")
  mixed <- omop_recipe(
    variables = list(binary, count),
    outputs = omop_output(name = "tc", type = "temporal_covariates"))
  expect_error(recipe_to_plan(mixed, version = 1), "Cartesian product")
  # A-14: the v1 refusal is a newly permitted shape; the v2 rendering computes
  # exactly the declared pairs 1/binary and 2/count (per-source analyses),
  # never their product.
  e <- tryCatch(recipe_to_plan(mixed, version = 1), error = function(e) e)
  expect_s3_class(e, "dsomop_v1_newly_permitted")
  plan <- recipe_to_plan(mixed)
  tc <- plan$outputs$tc$options
  srcs <- vapply(tc$sources, function(r) r$event, "")
  out_analyses <- unlist(tc$analyses)
  expect_identical(out_analyses, c("binary", "count"))
  pairs <- unlist(lapply(srcs, function(nm) {
    selected <- unlist(tc$source_analyses[[nm]]) %||% out_analyses
    as.vector(outer(unlist(plan$events[[nm]]$concept_set$concepts), selected, paste, sep = "/"))
  }))
  expect_setequal(pairs, c("1/binary", "2/count"))
  expect_s3_class(dsOMOPClient:::.recipe_plan_auto(mixed), "omop_plan_v2")
  # an analysis no variable requests is refused rather than invented
  mixed$outputs$tc$options <- list(analyses = c("binary", "count"))
  only_binary <- mixed
  only_binary$variables <- list(presence = binary)
  expect_error(recipe_to_plan(only_binary), "no variable requests")

  unscoped <- binary
  unscoped$concept_id <- NULL
  dynamic <- recipe_to_plan(omop_recipe(
    variables = unscoped,
    outputs = omop_output(name = "tc", type = "temporal_covariates")), version = 1)
  expect_null(dynamic$outputs$tc$concept_set)
  expect_identical(dynamic$outputs$tc$analyses, "binary")

  complete <- omop_recipe(
    variables = list(
      binary,
      omop_variable(name = "one_count", table = "measurement",
                    concept_id = 1, format = "count"),
      omop_variable(name = "two_presence", table = "measurement",
                    concept_id = 2, format = "binary"),
      count),
    outputs = omop_output(name = "tc", type = "temporal_covariates"))
  expect_equal(recipe_to_plan(complete, version = 1)$outputs$tc$analyses,
               c("binary", "count"))
})

test_that("temporal analyses survive a JSON recipe round-trip", {
  recipe <- omop_recipe(
    variables = list(
      omop_variable(
        name = "presence", table = "measurement",
        concept_id = 1, format = "binary"
      ),
      omop_variable(
        name = "records", table = "measurement",
        concept_id = 1, format = "count"
      )
    ),
    outputs = omop_output(
      name = "tc", type = "temporal_covariates",
      options = list(analyses = c("binary", "count"))
    )
  )

  restored <- recipe_import_json(recipe_export_json(recipe))
  expect_identical(restored$outputs$tc$options$analyses,
                   c("binary", "count"))
  expect_identical(recipe_to_plan(restored, version = 1)$outputs$tc$analyses,
                   c("binary", "count"))
})

test_that("longitudinal recipes preserve descendant concept scope", {
  expanded <- omop_variable(
    name = "dx", table = "condition_occurrence", concept_id = 201820,
    format = "raw", expand = TRUE
  )

  intervals <- recipe_to_plan(omop_recipe(
    variables = expanded,
    outputs = omop_output(name = "iv", type = "intervals")
  ), version = 1)
  expect_true(
    intervals$outputs$iv$concept_filter$condition_occurrence$
      include_descendants
  )

  survival <- recipe_to_plan(omop_recipe(
    variables = expanded,
    outputs = omop_output(name = "tte", type = "survival")
  ), version = 1)
  expect_true(
    survival$outputs$tte$outcome$concept_set$include_descendants
  )

  temporal_variable <- expanded
  temporal_variable$format <- "binary"
  temporal <- recipe_to_plan(omop_recipe(
    variables = temporal_variable,
    outputs = omop_output(name = "tc", type = "temporal_covariates")
  ), version = 1)
  expect_true(temporal$outputs$tc$concept_set$include_descendants)
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
                                    output_mode = "memory",
                                    on_small_site = c("error", "skip")) {
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
