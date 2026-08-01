# ==============================================================================
# Tests for client profiling wrappers
# ==============================================================================

test_that("ds.omop.table.stats has expected signature with new params", {
  args <- formals(ds.omop.table.stats)
  expect_true("scope" %in% names(args))
  expect_true("pooling_policy" %in% names(args))
  expect_true("execute" %in% names(args))
  expect_true(args$execute)
})

test_that("ds.omop.table.stats execute=FALSE returns dsomop_result", {
  result <- ds.omop.table.stats("person", execute = FALSE)
  expect_s3_class(result, "dsomop_result")
  expect_equal(length(result$per_site), 0)
  expect_null(result$pooled)
  expect_true(grepl("ds.omop.table.stats", result$meta$call_code))
  expect_true(grepl("person", result$meta$call_code))
})

test_that("ds.omop.column.stats execute=FALSE returns dsomop_result", {
  result <- ds.omop.column.stats("person", "year_of_birth", execute = FALSE)
  expect_s3_class(result, "dsomop_result")
  expect_true(grepl("ds.omop.column.stats", result$meta$call_code))
  expect_true(grepl("year_of_birth", result$meta$call_code))
})

test_that("ds.omop.domain.coverage execute=FALSE returns dsomop_result", {
  result <- ds.omop.domain.coverage(execute = FALSE)
  expect_s3_class(result, "dsomop_result")
  expect_true(grepl("ds.omop.domain.coverage", result$meta$call_code))
})

test_that("ds.omop.missingness execute=FALSE returns dsomop_result", {
  result <- ds.omop.missingness("person", execute = FALSE)
  expect_s3_class(result, "dsomop_result")
  expect_true(grepl("ds.omop.missingness", result$meta$call_code))
})

test_that("ds.omop.value.counts execute=FALSE returns dsomop_result", {
  result <- ds.omop.value.counts("person", "gender_concept_id", execute = FALSE)
  expect_s3_class(result, "dsomop_result")
  expect_true(grepl("ds.omop.value.counts", result$meta$call_code))
})

test_that("ds.omop.column.stats accepts a concept_id and plumbs it through", {
  args <- formals(ds.omop.column.stats)
  expect_true("concept_id" %in% names(args))
  expect_null(args$concept_id)

  res_all <- ds.omop.column.stats("measurement", "value_as_number",
                                  execute = FALSE)
  expect_false(grepl("concept_id", res_all$meta$call_code))

  res_one <- ds.omop.column.stats("measurement", "value_as_number",
                                  concept_id = 3004410, execute = FALSE)
  expect_s3_class(res_one, "dsomop_result")
  expect_true(grepl("concept_id = 3004410", res_one$meta$call_code))
})

test_that("ds.omop.value.counts accepts a concept_id and plumbs it through", {
  args <- formals(ds.omop.value.counts)
  expect_true("concept_id" %in% names(args))
  expect_null(args$concept_id)

  res_all <- ds.omop.value.counts("measurement", "value_as_concept_id",
                                  execute = FALSE)
  expect_false(grepl("concept_id = ", res_all$meta$call_code))

  res_one <- ds.omop.value.counts("measurement", "value_as_concept_id",
                                  concept_id = 3004410, execute = FALSE)
  expect_s3_class(res_one, "dsomop_result")
  expect_true(grepl("concept_id = 3004410", res_one$meta$call_code))
})

test_that("all profiling functions have scope parameter", {
  fns <- list(
    ds.omop.table.stats,
    ds.omop.column.stats,
    ds.omop.domain.coverage,
    ds.omop.missingness,
    ds.omop.value.counts
  )
  for (fn in fns) {
    args <- formals(fn)
    expect_true("scope" %in% names(args))
    expect_true("execute" %in% names(args))
  }
})

test_that("strict profiling never publishes a partial federation", {
  symbol <- "profiling_partial_federation"
  assign(
    symbol,
    structure(
      list(
        conns = list(a = "FAKE_A", b = "FAKE_B"),
        res_symbol = "dsO.partial",
        server_names = c("a", "b")
      ),
      class = "omop_session"
    ),
    envir = dsOMOPClient:::.dsomop_client_env
  )
  on.exit(rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)

  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      if (identical(server, "b")) {
        stop("connection failed", call. = FALSE)
      }
      stats::setNames(list(data.frame(
        table_name = "condition_occurrence",
        n_records = 100,
        n_persons = 80
      )), server)
    },
    .package = "DSI"
  )

  strict <- ds.omop.domain.coverage(
    scope = "pooled", pooling_policy = "strict", symbol = symbol
  )
  expect_null(strict$pooled)
  expect_named(attr(strict$per_site, "ds_errors"), "b")
  expect_true(any(grepl("incomplete federation", strict$meta$warnings)))

  permissive <- ds.omop.domain.coverage(
    scope = "pooled", pooling_policy = "pooled_only_ok", symbol = symbol
  )
  expect_equal(permissive$pooled$n_records, 100)
  expect_equal(permissive$pooled$n_persons, 80)
})
