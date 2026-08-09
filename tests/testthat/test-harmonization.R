.harm_settings <- function(breaks = seq(0, 85, 5), band = 5,
                           age_min = 5, date_min = 30,
                           version = "dsomop-harmonization-v3",
                           timezone = "UTC",
                           max_feature_specs = 1000,
                           max_pivot_concepts = 1000,
                           max_output_columns = 5000,
                           max_temporal_bins = 10000,
                           max_events_per_group = 100,
                           max_filter_depth = 32,
                           max_filter_nodes = 1024,
                           max_filter_values = 10000,
                           max_plan_outputs = 100,
                           max_analysis_scope_tables = 8) {
  list(
    harmonization_contract_version = version,
    age_breaks = breaks,
    age_semantics = "reference_year_minus_year_of_birth",
    date_semantics = "ISO8601_Gregorian_closed_interval",
    date_granularity = "calendar_day",
    datetime_timezone = timezone,
    week_start = "Monday",
    nfilter_age_range = age_min,
    nfilter_date_range = date_min,
    nfilter_band = band,
    max_feature_specs = max_feature_specs,
    max_pivot_concepts = max_pivot_concepts,
    max_output_columns = max_output_columns,
    max_temporal_bins = max_temporal_bins,
    max_events_per_group = max_events_per_group,
    max_filter_depth = max_filter_depth,
    max_filter_nodes = max_filter_nodes,
    max_filter_values = max_filter_values,
    max_plan_outputs = max_plan_outputs,
    max_analysis_scope_tables = max_analysis_scope_tables
  )
}

test_that("federated contract uses common coarsening and restrictive floors", {
  settings <- list(
    a = .harm_settings(seq(0, 85, 5), age_min = 5, date_min = 30),
    b = .harm_settings(seq(0, 90, 10), age_min = 10, date_min = 60)
  )
  contract <- .federated_harmonization_contract(settings)

  expect_equal(contract$common_age_breaks, seq(0, 80, 10))
  expect_equal(contract$common_age_groups,
               c(paste0(seq(0, 70, 10), "-", seq(9, 79, 10)), "80+"))
  expect_equal(contract$min_age_range_years, 10)
  expect_equal(contract$min_date_range_days, 60)
  expect_false(contract$age_grids_identical)
  expect_true(contract$poolable_counts)
  expect_identical(contract$datetime_timezone, "UTC")
})

test_that("v3 federations negotiate every operational cap by minimum", {
  settings <- list(
    a = .harm_settings(max_feature_specs = 80, max_pivot_concepts = 70,
                       max_output_columns = 600, max_temporal_bins = 500,
                       max_events_per_group = 20,
                       max_analysis_scope_tables = 6),
    b = .harm_settings(max_feature_specs = 40, max_pivot_concepts = 50,
                       max_output_columns = 300, max_temporal_bins = 200,
                       max_events_per_group = 8,
                       max_analysis_scope_tables = 3)
  )
  contract <- .federated_harmonization_contract(settings)

  expect_equal(contract$max_feature_specs, 40)
  expect_equal(contract$max_pivot_concepts, 50)
  expect_equal(contract$max_output_columns, 300)
  expect_equal(contract$max_temporal_bins, 200)
  expect_equal(contract$max_events_per_group, 8)
  expect_equal(contract$max_filter_depth, 32)
  expect_equal(contract$max_filter_nodes, 1024)
  expect_equal(contract$max_filter_values, 10000)
  expect_equal(contract$max_plan_outputs, 100)
  expect_equal(contract$max_analysis_scope_tables, 3)
  expect_true(contract$resource_caps_compatible)

  settings$b$max_temporal_bins <- NULL
  expect_error(.federated_harmonization_contract(settings),
               "operational caps are missing or invalid")
  expect_error(.federated_harmonization_contract(list(
    a = .harm_settings(version = "dsomop-harmonization-v2"),
    b = .harm_settings(version = "dsomop-harmonization-v2"))),
    "contract versions differ")
})

test_that("missing or semantically different site metadata fails closed", {
  settings <- list(a = .harm_settings(),
                   b = .harm_settings(version = "future-contract"))
  expect_error(.federated_harmonization_contract(settings),
               "versions differ")
  expect_error(.federated_harmonization_contract(settings["a"],
                                                   c("a", "b")),
               "missing disclosure metadata")
  expect_error(.federated_harmonization_contract(list(
    a = .harm_settings(timezone = "UTC"),
    b = .harm_settings(timezone = "Europe/Madrid"))),
    "semantics or contract versions differ")
})

test_that("plan validation applies common age and date policy before execution", {
  contract <- .federated_harmonization_contract(list(
    a = .harm_settings(seq(0, 85, 5), date_min = 30),
    b = .harm_settings(seq(0, 90, 10), age_min = 10, date_min = 60)
  ))

  expect_error(.validate_plan_harmonization(list(filters = list(
    type = "age_range", params = list(min = 20, max = 24))), contract),
    "10-year")
  expect_invisible(.validate_plan_harmonization(list(filters = list(
    type = "age_group", params = list(groups = "0-19"))), contract))
  expect_error(.validate_plan_harmonization(list(filters = list(
    type = "age_group", params = list(groups = "5-14"))), contract),
    "common age grid")
  expect_error(.validate_plan_harmonization(list(filters = list(
    type = "date_range",
    params = list(start = "2024-01-01", end = "2024-01-30"))), contract),
    "60-day")
  expect_invisible(.validate_plan_harmonization(list(filters = list(
    type = "has_concept", params = list(window = list(start = -29, end = 0)))),
    contract))
  expect_invisible(.validate_plan_harmonization(list(temporal = list(
    index_window = list(start = 0, end = 0))), contract))
  expect_error(.validate_plan_harmonization(list(temporal = list(
    calendar = list(start = "2024-01-01", end = "2024-01-30"))), contract),
    "bounded calendar window")

  # Numeric annual-resolution age is grid-independent once semantics match.
  exact_age_output <- list(outputs = list(x = list(
    type = "person_level", derived_columns = list(list(kind = "age")))))
  expect_invisible(.validate_plan_harmonization(exact_age_output, contract))

  age_output <- list(outputs = list(x = list(
    type = "baseline", derived = "age_at_index")))
  expect_error(.validate_plan_harmonization(age_output, contract),
               "negotiated common age grid")
  harmonized_age <- .apply_plan_harmonization(age_output, contract)
  expect_equal(harmonized_age$outputs$x$age_breaks,
               contract$common_age_breaks)
  expect_invisible(.validate_plan_harmonization(harmonized_age, contract))
})

test_that("plan validation enforces negotiated v3 caps recursively", {
  contract <- .federated_harmonization_contract(list(
    a = .harm_settings(max_feature_specs = 2, max_pivot_concepts = 2,
                       max_output_columns = 20, max_temporal_bins = 2),
    b = .harm_settings(max_feature_specs = 8, max_pivot_concepts = 8,
                       max_output_columns = 20, max_temporal_bins = 20)
  ))
  specs <- replicate(3, list(type = "count"), simplify = FALSE)

  expect_error(.validate_plan_harmonization(list(outputs = list(
    nested = list(type = "person_level", tables = list(
      measurement = list(features = specs))))), contract),
    "max_feature_specs")
  expect_error(.validate_plan_harmonization(list(outputs = list(
    nested = list(type = "event_level", concept_set = 1:3,
                  representation = list(format = "sparse")))), contract),
    "max_pivot_concepts")

  two_specs <- replicate(2, list(type = "count"), simplify = FALSE)
  output_contract <- .federated_harmonization_contract(list(
    a = .harm_settings(max_feature_specs = 10, max_output_columns = 2),
    b = .harm_settings(max_feature_specs = 10, max_output_columns = 4)
  ))
  expect_error(.validate_plan_harmonization(list(outputs = list(
    nested = list(type = "person_level", tables = list(
      measurement = list(features = two_specs))))), output_contract),
    "max_output_columns")
  expect_error(.validate_plan_harmonization(list(outputs = list(
    nested = list(type = "event_level", concept_set = 1L,
                  representation = list(format = "features",
                                        features = list())))),
    output_contract), "max_output_columns")
  expect_error(.validate_plan_harmonization(list(outputs = list(
    nested = list(type = "event_level", concept_set = 1:2,
                  representation = list(format = "wide")))),
    output_contract), "max_output_columns")

  expect_error(.validate_plan_harmonization(list(outputs = list(
    nested = list(type = "temporal_covariates", concept_set = 1:2,
                  bin_width = 10, window_start = -20, window_end = 0))),
    contract), "max_temporal_bins")
  expect_error(.validate_plan_harmonization(list(outputs = list(
    nested = list(type = "person_period", concept_set = 1:2,
                  grain = "episode", time_origin = "index",
                  bin_width = 10, window_start = -20, window_end = 0))),
    contract), "max_temporal_bins")
  interval_contract <- .federated_harmonization_contract(list(
    a = .harm_settings(max_events_per_group = 3),
    b = .harm_settings(max_events_per_group = 2)
  ))
  expect_error(.validate_plan_harmonization(list(outputs = list(
    intervals = list(type = "intervals_long", select_n = 3L)
  )), interval_contract), "max_events_per_group")
  expect_invisible(.validate_plan_harmonization(list(outputs = list(
    nested = list(type = "temporal_covariates", concept_set = 1:2,
                  bin_width = 10, window_start = -10, window_end = 0))),
    contract))

  scope_contract <- .federated_harmonization_contract(list(
    a = .harm_settings(max_analysis_scope_tables = 2),
    b = .harm_settings(max_analysis_scope_tables = 1)
  ))
  expect_error(.validate_plan_harmonization(list(
    scope = list(tables = c("scope_a", "scope_b")), outputs = list()
  ), scope_contract), "max_analysis_scope_tables")

  complexity <- .federated_harmonization_contract(list(
    a = .harm_settings(max_filter_depth = 10, max_filter_nodes = 2,
                       max_filter_values = 2, max_plan_outputs = 1),
    b = .harm_settings(max_filter_depth = 10, max_filter_nodes = 5,
                       max_filter_values = 5, max_plan_outputs = 5)
  ))
  leaf <- list(type = "sex", params = list(value = "F"))
  expect_error(.validate_plan_harmonization(list(outputs = list(
    a = list(), b = list())), complexity), "max_plan_outputs")
  expect_error(.validate_plan_harmonization(list(
    cohort = list(filter_tree = list(and = list(leaf, leaf))),
    outputs = list()), complexity), "max_filter_nodes")
  expect_error(.validate_plan_harmonization(list(
    cohort = list(filter_tree = list(
      type = "has_concept", params = list(concept_ids = 1:3))),
    outputs = list()), complexity), "max_filter_values")
})

test_that("count pooling rejects different server band widths", {
  settings <- list(a = .harm_settings(band = 5),
                   b = .harm_settings(band = 10))
  contract <- .federated_harmonization_contract(settings)
  expect_false(contract$poolable_counts)
  out <- .pool_result(
    list(a = list(rows = 45, persons = 40),
         b = list(rows = 40, persons = 40)),
    "table_stats", "strict", harmonization = contract)
  expect_null(out$result)
  expect_match(out$warnings, "incompatible count-band")
})

test_that("compatible banded pooling is labelled as approximate", {
  contract <- .federated_harmonization_contract(list(
    a = .harm_settings(band = 5), b = .harm_settings(band = 5)))
  out <- .pool_result(
    list(a = list(rows = 45, persons = 40),
         b = list(rows = 40, persons = 35)),
    "table_stats", "strict", harmonization = contract)
  expect_equal(out$result$rows, 85)
  expect_match(out$warnings, "banded lower bounds")
})

test_that("deprecated query pool cannot bypass the count-band contract", {
  settings <- list(a = .harm_settings(band = 5),
                   b = .harm_settings(band = 10))
  results <- list(
    a = data.frame(group = "x", n = 45),
    b = data.frame(group = "x", n = 40))
  attr(results, "dsomop.pooling_contract") <- list(
    version = 1L, strategy = "tabular",
    columns = list(group = list(role = "key"), n = list(role = "sum"))
  )
  attr(results, "dsomop.harmonization") <-
    .federated_harmonization_contract(settings)
  attr(results, "dsomop.analysis_name") <- "dsomop:test"
  attr(results, "dsomop.expected_servers") <- c("a", "b")
  expect_error(
    suppressWarnings(ds.omop.query.pool(results)),
    "count-band settings are incompatible")
})

test_that("date counts align by exact period, never row position", {
  a <- data.frame(period = c("2022", "2023"),
                  n_records = c(10, 20), n_persons = c(5, 10))
  b <- data.frame(period = c("2023", "2024"),
                  n_records = c(30, 40), n_persons = c(15, 20))

  strict <- .pool_result(list(a = a, b = b), "date_counts", "strict")
  expect_null(strict$result)
  expect_match(strict$warnings, "different calendar periods")

  common <- .pool_result(list(a = a, b = b), "date_counts",
                         "pooled_only_ok")
  expect_equal(common$result$period, "2023")
  expect_equal(common$result$n_records, 50)
  expect_equal(common$result$n_persons, 25)

  # Row order is irrelevant once the actual period key matches.
  c_site <- data.frame(period = c("2023", "2022"),
                       n_records = c(30, 40), n_persons = c(15, 20))
  aligned <- .pool_result(list(a = a, c = c_site),
                          "date_counts", "strict")
  expect_equal(aligned$result$period, c("2022", "2023"))
  expect_equal(aligned$result$n_records, c(50, 50))
})
