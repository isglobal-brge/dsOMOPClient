# ==============================================================================
# Tests for dsomop_result S3 class
# ==============================================================================

test_that("dsomop_result() constructor returns correct class", {
  r <- dsomop_result(
    per_site = list(server_a = data.frame(x = 1:3)),
    pooled = NULL,
    meta = list(call_code = "ds.omop.table.stats(\"person\")", scope = "per_site")
  )
  expect_s3_class(r, "dsomop_result")
  expect_true(inherits(r, "list"))
  expect_equal(r$meta$scope, "per_site")
  expect_equal(r$meta$servers, "server_a")
  expect_true(nchar(r$meta$call_code) > 0)
  expect_true(inherits(r$meta$timestamp, "POSIXct"))
})

test_that("dsomop_result with pooled data stores correctly", {
  r <- dsomop_result(
    per_site = list(a = list(rows = 100), b = list(rows = 200)),
    pooled = list(rows = 300),
    meta = list(call_code = "test()", scope = "pooled",
                pooling_policy = "strict")
  )
  expect_equal(r$pooled$rows, 300)
  expect_equal(r$meta$scope, "pooled")
  expect_equal(r$meta$pooling_policy, "strict")
  expect_equal(length(r$meta$servers), 2)
})

test_that("$ operator falls through to per_site for backward compat", {
  r <- dsomop_result(
    per_site = list(server_a = data.frame(x = 1:3),
                    server_b = data.frame(x = 4:6))
  )
  # Direct top-level access

  expect_true(is.list(r$per_site))
  expect_null(r$pooled)
  expect_true(is.list(r$meta))

  # Fall through to per_site
  expect_true(is.data.frame(r$server_a))
  expect_equal(r$server_a$x, 1:3)
  expect_true(is.data.frame(r$server_b))
  expect_equal(r$server_b$x, 4:6)
})

test_that("ds.omop.code() extracts code string", {
  r <- dsomop_result(
    per_site = list(a = 1),
    meta = list(call_code = 'ds.omop.table.stats(table = "person")')
  )
  code <- ds.omop.code(r)
  expect_equal(code, 'ds.omop.table.stats(table = "person")')
})

test_that("ds.omop.code() errors on non-dsomop_result", {
  expect_error(ds.omop.code(list(a = 1)), "dsomop_result")
})

test_that("print.dsomop_result produces output", {
  r <- dsomop_result(
    per_site = list(a = 1, b = 2),
    pooled = list(total = 3),
    meta = list(call_code = "test()", scope = "pooled",
                warnings = c("some warning"))
  )
  out <- capture.output(print(r))
  expect_true(any(grepl("dsomop_result", out)))
  expect_true(any(grepl("a, b", out)))
  expect_true(any(grepl("pooled", out, ignore.case = TRUE)))
  expect_true(any(grepl("Warnings", out)))
})

test_that("as.data.frame.dsomop_result returns pooled when available", {
  pooled_df <- data.frame(table_name = "person", n_records = 500)
  r <- dsomop_result(
    per_site = list(a = data.frame(table_name = "person", n_records = 200),
                    b = data.frame(table_name = "person", n_records = 300)),
    pooled = pooled_df
  )
  df <- as.data.frame(r)
  expect_equal(nrow(df), 1)
  expect_equal(df$n_records, 500)
})

test_that("as.data.frame.dsomop_result returns first server when no pooled", {
  r <- dsomop_result(
    per_site = list(a = data.frame(x = 1:3), b = data.frame(x = 4:6))
  )
  df <- as.data.frame(r)
  expect_equal(df$x, 1:3)
})

test_that("as.data.frame.dsomop_result returns empty data.frame when empty", {
  r <- dsomop_result(per_site = list())
  df <- as.data.frame(r)
  expect_equal(nrow(df), 0)
})

test_that("dsomop_result meta has default values", {
  r <- dsomop_result(per_site = list(a = 1))
  expect_equal(r$meta$call_code, "")
  expect_equal(r$meta$scope, "per_site")
  expect_equal(r$meta$pooling_policy, "strict")
  expect_equal(r$meta$warnings, character(0))
})
