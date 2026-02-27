# ==============================================================================
# Tests for client Achilles wrappers
# ==============================================================================

# --- Function signature tests -------------------------------------------------

test_that("ds.omop.achilles.status has expected signature", {
  expect_true(is.function(ds.omop.achilles.status))
  args <- formals(ds.omop.achilles.status)
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
})

test_that("ds.omop.achilles.analyses has expected signature", {
  expect_true(is.function(ds.omop.achilles.analyses))
  args <- formals(ds.omop.achilles.analyses)
  expect_true("domain" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_null(args$domain)
})

test_that("ds.omop.achilles.results has expected signature", {
  expect_true(is.function(ds.omop.achilles.results))
  args <- formals(ds.omop.achilles.results)
  expect_true("analysis_ids" %in% names(args))
  expect_true("scope" %in% names(args))
  expect_true("pooling_policy" %in% names(args))
  expect_true("execute" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_true(args$execute)
  # Fix C/D: stratum_filters and min_cell_count intentionally removed
  expect_false("stratum_filters" %in% names(args))
  expect_false("min_cell_count" %in% names(args))
})

test_that("ds.omop.achilles.distribution has expected signature", {
  expect_true(is.function(ds.omop.achilles.distribution))
  args <- formals(ds.omop.achilles.distribution)
  expect_true("analysis_ids" %in% names(args))
  expect_true("scope" %in% names(args))
  expect_true("execute" %in% names(args))
  # Fix C: stratum_filters intentionally removed
  expect_false("stratum_filters" %in% names(args))
})

# --- execute=FALSE dry-run tests ----------------------------------------------

test_that("ds.omop.achilles.results execute=FALSE returns dsomop_result", {
  result <- ds.omop.achilles.results(
    analysis_ids = c(0, 1, 400),
    execute = FALSE
  )
  expect_s3_class(result, "dsomop_result")
  expect_equal(length(result$per_site), 0)
  expect_null(result$pooled)
  expect_true(nchar(result$meta$call_code) > 0)
  expect_true(grepl("ds.omop.achilles.results", result$meta$call_code))
})

test_that("ds.omop.achilles.distribution execute=FALSE returns dsomop_result", {
  result <- ds.omop.achilles.distribution(
    analysis_ids = c(3, 113),
    execute = FALSE
  )
  expect_s3_class(result, "dsomop_result")
  expect_equal(length(result$per_site), 0)
  expect_true(grepl("ds.omop.achilles.distribution", result$meta$call_code))
})

test_that("ds.omop.achilles.results generates correct call_code", {
  result <- ds.omop.achilles.results(
    analysis_ids = c(400, 401),
    scope = "pooled",
    execute = FALSE
  )
  code <- result$meta$call_code
  expect_true(grepl("ds.omop.achilles.results", code))
  expect_true(grepl("400", code))
  expect_true(grepl("pooled", code))
})

# --- Pooling unit tests -------------------------------------------------------

test_that("pooling achilles_results sums counts correctly", {
  per_site <- list(
    server_a = data.frame(
      analysis_id = c(0L, 1L, 1L),
      stratum_1 = c(NA, "8507", "8532"),
      stratum_2 = NA_character_,
      count_value = c(100, 55, 45),
      stringsAsFactors = FALSE
    ),
    server_b = data.frame(
      analysis_id = c(0L, 1L, 1L),
      stratum_1 = c(NA, "8507", "8532"),
      stratum_2 = NA_character_,
      count_value = c(200, 110, 90),
      stringsAsFactors = FALSE
    )
  )

  result <- .pool_result(per_site, "achilles_results", "strict")
  expect_false(is.null(result$result))
  df <- result$result

  total <- df[df$analysis_id == 0, ]
  expect_equal(total$count_value, 300)

  male <- df[df$analysis_id == 1 & df$stratum_1 == "8507", ]
  expect_equal(male$count_value, 165)

  female <- df[df$analysis_id == 1 & df$stratum_1 == "8532", ]
  expect_equal(female$count_value, 135)
})

test_that("pooling achilles_results strict policy fails on NA", {
  per_site <- list(
    server_a = data.frame(
      analysis_id = 0L, stratum_1 = NA_character_,
      stratum_2 = NA_character_,
      count_value = 100, stringsAsFactors = FALSE
    ),
    server_b = data.frame(
      analysis_id = 0L, stratum_1 = NA_character_,
      stratum_2 = NA_character_,
      count_value = NA_real_, stringsAsFactors = FALSE
    )
  )

  result <- .pool_result(per_site, "achilles_results", "strict")
  # Fix E: suppressed rows are DROPPED from pooled output (no hints)
  df <- result$result
  expect_equal(nrow(df[df$analysis_id == 0, , drop = FALSE]), 0)
})

test_that("pooling achilles_distribution weighted mean calculation", {
  per_site <- list(
    server_a = data.frame(
      analysis_id = 113L,
      stratum_1 = NA_character_,
      stratum_2 = NA_character_,
      count_value = 100,
      min_value = 20, max_value = 80,
      avg_value = 50.0, stdev_value = 10.0,
      median_value = 50, p10_value = 30, p25_value = 40,
      p75_value = 60, p90_value = 70,
      stringsAsFactors = FALSE
    ),
    server_b = data.frame(
      analysis_id = 113L,
      stratum_1 = NA_character_,
      stratum_2 = NA_character_,
      count_value = 200,
      min_value = 18, max_value = 90,
      avg_value = 55.0, stdev_value = 12.0,
      median_value = 55, p10_value = 28, p25_value = 38,
      p75_value = 65, p90_value = 75,
      stringsAsFactors = FALSE
    )
  )

  result <- .pool_result(per_site, "achilles_distribution", "strict")
  expect_false(is.null(result$result))
  df <- result$result

  # Pooled count
  expect_equal(df$count_value, 300)

  # Weighted mean: (100*50 + 200*55) / 300 = 16000/300 = 53.33
  expect_equal(round(df$avg_value, 2), 53.33)

  # Fix F: min/max no longer pooled (disclosure control)
  expect_true(is.null(df$min_value) || is.na(df$min_value))
  expect_true(is.null(df$max_value) || is.na(df$max_value))

  # Stdev should be computed (Cochrane formula)
  expect_true(!is.na(df$stdev_value))
  expect_true(df$stdev_value > 0)
})

test_that("pooling achilles_distribution sets median/percentiles to NA", {
  per_site <- list(
    server_a = data.frame(
      analysis_id = 113L,
      stratum_1 = NA_character_,
      stratum_2 = NA_character_,
      count_value = 100,
      min_value = 20, max_value = 80,
      avg_value = 50.0, stdev_value = 10.0,
      median_value = 50, p10_value = 30, p25_value = 40,
      p75_value = 60, p90_value = 70,
      stringsAsFactors = FALSE
    )
  )

  result <- .pool_result(per_site, "achilles_distribution", "strict")
  df <- result$result

  # Median and percentiles should be NA (not poolable)
  expect_true(is.na(df$median_value))
  expect_true(is.na(df$p10_value))
  expect_true(is.na(df$p25_value))
  expect_true(is.na(df$p75_value))
  expect_true(is.na(df$p90_value))

  # Warnings should mention percentiles
  expect_true(any(grepl("percentiles", result$warnings)))
})

# --- Catalog client function ---------------------------------------------------

test_that("ds.omop.achilles.catalog has expected signature", {
  expect_true(is.function(ds.omop.achilles.catalog))
  args <- formals(ds.omop.achilles.catalog)
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
})
