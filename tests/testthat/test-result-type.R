test_that("result type aliases follow the shared dsBaseClient-style contract", {
  aliases <- c(
    split = "split", splits = "split", s = "split", per_site = "split",
    combine = "combine", combined = "combine", c = "combine",
    pooled = "combine", both = "both", b = "both"
  )
  for (alias in names(aliases)) {
    expect_identical(.normalize_result_type(alias), unname(aliases[[alias]]))
  }
  expect_error(.normalize_result_type("site-ish"), "type must be")
})

test_that("aggregate families expose split, combine, and both views", {
  raw <- list(
    site_a = data.frame(key = "x", count_value = 10),
    site_b = data.frame(key = "x", count_value = 15)
  )
  pooled <- data.frame(key = "x", count_value = 25)
  ohdsi_contract <- list(
    contract_version = 1L,
    tool_id = "cohort_diagnostics",
    table_name = "cohort_count",
    pooling_contract = list(
      version = 1L, strategy = "tabular",
      columns = list(
        key = list(role = "key"),
        count_value = list(role = "sum")
      )
    )
  )

  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(site_a = "A", site_b = "B"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) {
      if (identical(as.character(expr[[1L]]),
                    "omopOhdsiResultContractDS")) {
        return(list(site_a = ohdsi_contract, site_b = ohdsi_contract))
      }
      raw
    },
    .pool_result = function(per_site, result_type, pooling_policy) {
      list(result = pooled, warnings = character(0))
    },
    .pool_analysis_contract = function(...) {
      list(result = pooled, warnings = character(0))
    },
    .session_harmonization_for_connections = function(...) {
      list(poolable_counts = TRUE, count_band_width = 5L)
    },
    .package = "dsOMOPClient"
  )

  families <- list(
    profiling = function(type) ds.omop.table.stats("person", type = type),
    exploration = function(type) {
      ds.omop.concept.prevalence("condition_occurrence", type = type)
    },
    achilles = function(type) ds.omop.achilles.results(1L, type = type),
    ohdsi_results = function(type) {
      ds.omop.ohdsi.results("cohort_count", type = type)
    }
  )
  views <- list(
    s = list(type = "split", sites = 2L, pooled = FALSE),
    combined = list(type = "combine", sites = 0L, pooled = TRUE),
    b = list(type = "both", sites = 2L, pooled = TRUE)
  )

  for (family in names(families)) {
    for (alias in names(views)) {
      expected <- views[[alias]]
      result <- families[[family]](alias)
      expect_s3_class(result, "dsomop_result")
      expect_identical(names(result), c("per_site", "pooled", "meta"))
      expect_identical(result$meta$type, expected$type)
      expect_length(result$per_site, expected$sites)
      expect_identical(!is.null(result$pooled), expected$pooled)
      expect_identical(result$meta$servers, c("site_a", "site_b"))
    }
  }
})

test_that("legacy scope defaults stay unambiguous", {
  split <- ds.omop.table.stats("person", execute = FALSE)
  both <- ds.omop.table.stats("person", scope = "pooled", execute = FALSE)

  expect_identical(split$meta$type, "split")
  expect_identical(both$meta$type, "both")
  expect_error(
    ds.omop.table.stats(
      "person", scope = "per_site", type = "combine", execute = FALSE
    ),
    "conflicting result views"
  )
  expect_identical(
    .resolve_result_type(pool = TRUE, pool_missing = FALSE), "both"
  )
  expect_identical(
    .resolve_result_type(pool = FALSE, pool_missing = FALSE), "split"
  )
  expect_error(
    .resolve_result_type(
      type = "combine", pool = FALSE, pool_missing = FALSE
    ),
    "conflicting result views"
  )
})

test_that("non-poolable combine views fail closed with a reason", {
  raw <- list(
    site_a = list(breaks = c(0, 1, 2), counts = c(10, 10)),
    site_b = list(breaks = c(0, 1, 2), counts = c(12, 12))
  )
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(site_a = "A", site_b = "B"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) raw,
    .package = "dsOMOPClient"
  )

  result <- ds.omop.safe.cutpoints(
    "measurement", "value_as_number", type = "combine"
  )
  expect_s3_class(result, "dsomop_result")
  expect_length(result$per_site, 0L)
  expect_null(result$pooled)
  expect_match(result$meta$pooling_reason, "site-specific")
  expect_true(any(grepl("site-specific", result$meta$warnings)))
})

test_that("analysis metadata supports split inspection before harmonization", {
  raw <- list(
    site_a = data.frame(name = "dsomop:a"),
    site_b = data.frame(name = "dsomop:b")
  )
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(site_a = "A", site_b = "B"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) raw,
    .package = "dsOMOPClient"
  )

  split <- ds.omop.analysis.list(type = "split")
  expect_named(split$per_site, c("site_a", "site_b"))
  expect_null(split$pooled)
  expect_error(ds.omop.analysis.list(type = "both"),
               "differs across servers")
})
