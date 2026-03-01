# ==============================================================================
# Tests for client OHDSI Results wrappers
# ==============================================================================

# --- Function signature tests -------------------------------------------------

test_that("ds.omop.ohdsi.status has expected signature", {
  expect_true(is.function(ds.omop.ohdsi.status))
  args <- formals(ds.omop.ohdsi.status)
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
})

test_that("ds.omop.ohdsi.tables has expected signature", {
  expect_true(is.function(ds.omop.ohdsi.tables))
  args <- formals(ds.omop.ohdsi.tables)
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
})

test_that("ds.omop.ohdsi.results has expected signature", {
  expect_true(is.function(ds.omop.ohdsi.results))
  args <- formals(ds.omop.ohdsi.results)
  expect_true("table_name" %in% names(args))
  expect_true("columns" %in% names(args))
  expect_true("filters" %in% names(args))
  expect_true("order_by" %in% names(args))
  expect_true("limit" %in% names(args))
  expect_true("tool_id" %in% names(args))
  expect_true("scope" %in% names(args))
  expect_true("pooling_policy" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_null(args$columns)
  expect_null(args$filters)
  expect_null(args$order_by)
  expect_null(args$tool_id)
})

test_that("ds.omop.ohdsi.summary has expected signature", {
  expect_true(is.function(ds.omop.ohdsi.summary))
  args <- formals(ds.omop.ohdsi.summary)
  expect_true("tool_id" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
})

# --- Pooling tests -----------------------------------------------------------

test_that("ohdsi_results pooling sums count columns", {
  per_site <- list(
    server_a = data.frame(
      cohort_id = c(1L, 2L),
      cohort_entries = c(100L, 50L),
      cohort_subjects = c(80L, 40L),
      stringsAsFactors = FALSE
    ),
    server_b = data.frame(
      cohort_id = c(1L, 2L),
      cohort_entries = c(200L, 75L),
      cohort_subjects = c(150L, 60L),
      stringsAsFactors = FALSE
    )
  )

  result <- .pool_result(per_site, "ohdsi_results", "strict")
  expect_false(is.null(result$result))
  pooled <- result$result
  expect_s3_class(pooled, "data.frame")
  # Counts should be summed per cohort
  c1 <- pooled[pooled$cohort_id == 1L, ]
  expect_equal(c1$cohort_entries, 300L)
  expect_equal(c1$cohort_subjects, 230L)
})

test_that("ohdsi_results pooling propagates NA for suppressed counts", {
  per_site <- list(
    server_a = data.frame(
      cohort_id = 1L,
      cohort_entries = 100L,
      stringsAsFactors = FALSE
    ),
    server_b = data.frame(
      cohort_id = 1L,
      cohort_entries = NA_integer_,
      stringsAsFactors = FALSE
    )
  )

  result <- .pool_result(per_site, "ohdsi_results", "strict")
  # With strict policy and NA propagation, the row should be dropped
  # (all count cols NA -> row dropped)
  expect_true(is.null(result$result) || nrow(result$result) == 0)
})

test_that("ohdsi_results pooling sets rates to NA", {
  per_site <- list(
    server_a = data.frame(
      outcome_id = 1L,
      outcomes = 50L,
      incidence_rate = 15.0,
      stringsAsFactors = FALSE
    ),
    server_b = data.frame(
      outcome_id = 1L,
      outcomes = 30L,
      incidence_rate = 12.0,
      stringsAsFactors = FALSE
    )
  )

  result <- .pool_result(per_site, "ohdsi_results", "strict")
  expect_false(is.null(result$result))
  pooled <- result$result
  # Rate should be NA (cannot sum rates)
  expect_true(is.na(pooled$incidence_rate))
  # Count should be summed
  expect_equal(pooled$outcomes, 80L)
})

test_that("ohdsi_results pooling handles empty input", {
  per_site <- list(
    server_a = data.frame(stringsAsFactors = FALSE)
  )
  result <- .pool_result(per_site, "ohdsi_results", "strict")
  expect_true(is.null(result$result) ||
    (is.data.frame(result$result) && nrow(result$result) == 0))
})

test_that("ohdsi_results pooling handles DQD data", {
  per_site <- list(
    server_a = data.frame(
      check_name = "measurePersonCompleteness",
      category = "Completeness",
      num_violated_rows = 5L,
      num_denominator_rows = 100L,
      pct_violated_rows = 5.0,
      stringsAsFactors = FALSE
    ),
    server_b = data.frame(
      check_name = "measurePersonCompleteness",
      category = "Completeness",
      num_violated_rows = 10L,
      num_denominator_rows = 200L,
      pct_violated_rows = 5.0,
      stringsAsFactors = FALSE
    )
  )

  result <- .pool_result(per_site, "ohdsi_results", "strict")
  expect_false(is.null(result$result))
  pooled <- result$result
  # Count cols summed
  expect_equal(pooled$num_violated_rows, 15L)
  expect_equal(pooled$num_denominator_rows, 300L)
  # Percentage set to NA (it's a rate-like column)
  expect_true(is.na(pooled$pct_violated_rows))
})
