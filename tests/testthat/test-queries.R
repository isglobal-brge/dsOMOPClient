.query_pool_deprecation <- "'ds\\.omop\\.query\\.pool' is deprecated"

.contracted_query_results <- function() {
  out <- list(
    a = data.frame(concept_id = c(1L, 2L), n_persons = c(10, 20)),
    b = data.frame(concept_id = c(1L, 2L), n_persons = c(5, 15))
  )
  attr(out, "dsomop.pooling_contract") <- list(
    version = 1L, strategy = "tabular",
    columns = list(
      concept_id = list(role = "key"),
      n_persons = list(role = "sum")
    )
  )
  attr(out, "dsomop.harmonization") <- list(
    poolable_counts = TRUE, count_band_width = 1L
  )
  attr(out, "dsomop.analysis_name") <- "dsomop:condition_prevalence"
  attr(out, "dsomop.expected_servers") <- c("a", "b")
  out
}

test_that("deprecated query pooling uses only its server-owned contract", {
  results <- .contracted_query_results()
  expect_warning(
    pooled <- ds.omop.query.pool(
      results, query_id = "condition_prevalence", policy = "strict"
    ),
    regexp = .query_pool_deprecation
  )
  expect_equal(pooled$concept_id, c(1L, 2L))
  expect_equal(pooled$n_persons, c(15, 35))
})

test_that("query.exec carries the contract needed by its deprecated bridge", {
  contract <- attr(.contracted_query_results(), "dsomop.pooling_contract")
  local_mocked_bindings(
    ds.omop.analysis.run = function(..., type) {
      expect_identical(type, "split")
      result <- dsomop_result(
        per_site = list(a = data.frame(concept_id = 1L, n_persons = 10)),
        meta = list(servers = "a", type = "split")
      )
      result$meta$pooling_contract <- contract
      result$meta$harmonization <- list(
        poolable_counts = TRUE, count_band_width = 1L
      )
      result
    },
    .package = "dsOMOPClient"
  )
  expect_warning(
    result <- ds.omop.query.exec("condition_prevalence"),
    "deprecated"
  )
  expect_identical(attr(result, "dsomop.pooling_contract"), contract)
  expect_identical(attr(result, "dsomop.expected_servers"), "a")
})

test_that("deprecated query pooling rejects heuristic/manual pooling", {
  manual <- list(
    a = data.frame(group = "x", n = 10),
    b = data.frame(group = "x", n = 20)
  )
  expect_error(
    suppressWarnings(ds.omop.query.pool(manual)),
    "complete federation coverage"
  )

  results <- .contracted_query_results()
  expect_error(
    suppressWarnings(ds.omop.query.pool(results, sensitive_fields = "n_persons")),
    "no longer accepted"
  )
  expect_error(
    suppressWarnings(ds.omop.query.pool(results, pool_strategy = "none")),
    "no longer accepted"
  )
  expect_error(
    suppressWarnings(ds.omop.query.pool(results, query_id = "wrong_query")),
    "does not match"
  )
})

test_that("deprecated query pooling requires complete authentic context", {
  results <- .contracted_query_results()
  attr(results, "ds_errors") <- list(b = "unavailable")
  expect_error(
    suppressWarnings(ds.omop.query.pool(results)),
    "incomplete federated"
  )

  results <- .contracted_query_results()
  attr(results, "dsomop.pooling_contract") <- NULL
  expect_error(
    suppressWarnings(ds.omop.query.pool(results)),
    "no server-owned pooling contract"
  )

  results <- .contracted_query_results()
  attr(results, "dsomop.expected_servers") <- c("a", "b", "c")
  expect_error(
    suppressWarnings(ds.omop.query.pool(results)),
    "complete federation coverage"
  )
})

test_that("deprecated query pooling validates policy before returning empty", {
  expect_error(
    suppressWarnings(ds.omop.query.pool(list(), policy = "strcit")),
    "should be one of"
  )
  expect_warning(
    expect_null(ds.omop.query.pool(NULL)),
    regexp = .query_pool_deprecation
  )
})
