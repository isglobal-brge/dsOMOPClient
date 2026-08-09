# Regression tests for disclosure-safe multi-server pooling.  These focus on
# cases where a missing/suppressed site contribution must not be converted to
# zero, and on person-unit weighting for longitudinal OMOP tables.

test_that("strict histogram pooling never turns suppression into zero", {
  a <- data.frame(
    bin_start = c(0, 10), bin_end = c(10, 20),
    count = c(5, NA), suppressed = c(FALSE, TRUE)
  )
  b <- data.frame(
    bin_start = c(0, 10), bin_end = c(10, 20),
    count = c(8, 12), suppressed = c(FALSE, FALSE)
  )

  out <- .pool_histograms(list(a = a, b = b), "strict")

  expect_equal(out$result$count[[1]], 13)
  expect_true(is.na(out$result$count[[2]]))
  expect_true(out$result$suppressed[[2]])
})

test_that("column statistics use contributing persons for longitudinal means", {
  # Both sites contain ten people. Site A happens to have many more repeated
  # records, but .profileColumnStats first reduces to one value per person.
  per_site <- list(
    a = list(n_total = 100, n_persons = 10, n_missing = 0,
             mean = 0, sd = 1),
    b = list(n_total = 10, n_persons = 10, n_missing = 0,
             mean = 100, sd = 1)
  )

  out <- .pool_result(per_site, "column_stats", "strict")

  expect_equal(out$result$n_total, 110)
  expect_equal(out$result$n_persons, 20)
  expect_equal(out$result$mean, 50)
})

test_that("strict missingness pooling propagates suppression and absent groups", {
  a <- data.frame(
    column_name = c("value", "unit"), n_total = c(100, 100),
    n_missing = c(NA, 20), missing_rate = c(NA, 0.2)
  )
  b <- data.frame(
    column_name = "value", n_total = 200,
    n_missing = 30, missing_rate = 0.15
  )

  out <- .pool_result(list(a = a, b = b), "missingness", "strict")
  value <- out$result[out$result$column_name == "value", , drop = FALSE]
  unit <- out$result[out$result$column_name == "unit", , drop = FALSE]

  expect_true(is.na(value$n_missing))
  expect_true(is.na(value$missing_rate))
  expect_true(is.na(unit$n_total))
  expect_true(is.na(unit$n_missing))
})

test_that("strict domain coverage never treats an absent table as zero", {
  a <- data.frame(
    table_name = c("person", "measurement"),
    n_records = c(100, 20), n_persons = c(100, 10)
  )
  b <- data.frame(
    table_name = "person", n_records = 200, n_persons = 200
  )

  out <- .pool_result(list(a = a, b = b), "domain_coverage", "strict")
  person <- out$result[out$result$table_name == "person", , drop = FALSE]
  measurement <- out$result[
    out$result$table_name == "measurement", , drop = FALSE
  ]

  expect_equal(person$n_records, 300)
  expect_equal(person$n_persons, 300)
  expect_true(is.na(measurement$n_records))
  expect_true(is.na(measurement$n_persons))
})

test_that("strict Achilles pooling does not publish a site-missing stratum", {
  a <- data.frame(
    analysis_id = 1L, stratum_1 = c("shared", "only-a"),
    count_value = c(10, 20), stringsAsFactors = FALSE
  )
  b <- data.frame(
    analysis_id = 1L, stratum_1 = "shared", count_value = 30,
    stringsAsFactors = FALSE
  )

  out <- .pool_result(list(a = a, b = b), "achilles_results", "strict")

  expect_identical(out$result$stratum_1, "shared")
  expect_equal(out$result$count_value, 40)
})

test_that("strict concept locator omits site-missing concept/table groups", {
  a <- data.frame(
    concept_id = c(1, 2), table_name = "measurement",
    concept_column = "measurement_concept_id",
    n_records = c(10, 20), n_persons = c(8, 12)
  )
  b <- data.frame(
    concept_id = 1, table_name = "measurement",
    concept_column = "measurement_concept_id",
    n_records = 30, n_persons = 25
  )

  out <- .pool_result(list(a = a, b = b), "concept_locate", "strict")

  expect_identical(out$result$concept_id, 1)
  expect_equal(out$result$n_records, 40)
  expect_equal(out$result$n_persons, 33)
})

test_that("strict Achilles distributions omit a site-missing stratum", {
  make <- function(stratum, count, mean) data.frame(
    analysis_id = 103L, stratum_1 = stratum, count_value = count,
    avg_value = mean, stdev_value = 1,
    median_value = mean, p10_value = mean, p25_value = mean,
    p75_value = mean, p90_value = mean, stringsAsFactors = FALSE
  )
  a <- rbind(make("shared", 10, 2), make("only-a", 20, 3))
  b <- make("shared", 30, 4)

  out <- .pool_result(
    list(a = a, b = b), "achilles_distribution", "strict"
  )

  expect_identical(out$result$stratum_1, "shared")
  expect_equal(out$result$count_value, 40)
  expect_equal(out$result$avg_value, 3.5)
})

test_that("strict stratified crosstabs retain missing-site slices as suppressed", {
  slice <- function(n) list(
    counts = matrix(n, nrow = 1L, dimnames = list("yes", "exposed"))
  )
  per_site <- list(
    a = list(stratified = TRUE, stratify_by = "sex",
             strata = list(F = slice(10), M = slice(11))),
    b = list(stratified = TRUE, stratify_by = "sex",
             strata = list(F = slice(20)))
  )

  out <- .pool_crosstab(per_site, "strict")

  expect_equal(out$result$strata$F$counts[[1]], 30)
  expect_null(out$result$strata$M)
  expect_true(any(grepl("suppressed|empty", out$warnings)))
})
