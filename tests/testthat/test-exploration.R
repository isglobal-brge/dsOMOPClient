# ==============================================================================
# Tests for client exploration wrappers
# ==============================================================================

test_that("ds.omop.concept.drilldown has expected signature with new params", {
  expect_true(is.function(ds.omop.concept.drilldown))
  args <- formals(ds.omop.concept.drilldown)
  expect_true("table" %in% names(args))
  expect_true("concept_id" %in% names(args))
  expect_true("scope" %in% names(args))
  expect_true("pooling_policy" %in% names(args))
  expect_true("execute" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
  expect_true(args$execute)
})

test_that("ds.omop.concept.locate has expected signature with new params", {
  expect_true(is.function(ds.omop.concept.locate))
  args <- formals(ds.omop.concept.locate)
  expect_true("concept_ids" %in% names(args))
  expect_true("scope" %in% names(args))
  expect_true("pooling_policy" %in% names(args))
  expect_true("execute" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
  expect_true(args$execute)
})

test_that("ds.omop.concept.prevalence execute=FALSE returns dsomop_result", {
  result <- ds.omop.concept.prevalence(
    table = "condition_occurrence",
    metric = "persons",
    top_n = 50,
    execute = FALSE
  )
  expect_s3_class(result, "dsomop_result")
  expect_equal(length(result$per_site), 0)
  expect_null(result$pooled)
  expect_true(nchar(result$meta$call_code) > 0)
  expect_true(grepl("ds.omop.concept.prevalence", result$meta$call_code))
  expect_true(grepl("condition_occurrence", result$meta$call_code))
})

test_that("ds.omop.value.histogram execute=FALSE returns dsomop_result", {
  result <- ds.omop.value.histogram(
    table = "measurement",
    value_col = "value_as_number",
    bins = 30L,
    execute = FALSE
  )
  expect_s3_class(result, "dsomop_result")
  expect_equal(length(result$per_site), 0)
  expect_true(grepl("ds.omop.value.histogram", result$meta$call_code))
})

test_that("ds.omop.value.quantiles execute=FALSE returns dsomop_result", {
  result <- ds.omop.value.quantiles(
    table = "measurement",
    value_col = "value_as_number",
    execute = FALSE
  )
  expect_s3_class(result, "dsomop_result")
  expect_equal(length(result$per_site), 0)
  expect_true(grepl("ds.omop.value.quantiles", result$meta$call_code))
})

test_that("ds.omop.date.counts execute=FALSE returns dsomop_result", {
  result <- ds.omop.date.counts(
    table = "condition_occurrence",
    granularity = "year",
    execute = FALSE
  )
  expect_s3_class(result, "dsomop_result")
  expect_true(grepl("ds.omop.date.counts", result$meta$call_code))
})

test_that("ds.omop.concept.drilldown execute=FALSE returns dsomop_result", {
  result <- ds.omop.concept.drilldown(
    table = "condition_occurrence",
    concept_id = 201820,
    execute = FALSE
  )
  expect_s3_class(result, "dsomop_result")
  expect_true(grepl("ds.omop.concept.drilldown", result$meta$call_code))
  expect_true(grepl("201820", result$meta$call_code))
})

test_that("ds.omop.concept.locate execute=FALSE returns dsomop_result", {
  result <- ds.omop.concept.locate(
    concept_ids = c(201820, 255573),
    execute = FALSE
  )
  expect_s3_class(result, "dsomop_result")
  expect_true(grepl("ds.omop.concept.locate", result$meta$call_code))
})

test_that("all exploration functions have scope parameter", {
  fns <- list(
    ds.omop.concept.prevalence,
    ds.omop.value.histogram,
    ds.omop.value.quantiles,
    ds.omop.date.counts,
    ds.omop.concept.drilldown,
    ds.omop.concept.locate
  )
  for (fn in fns) {
    args <- formals(fn)
    expect_true("scope" %in% names(args),
      info = paste("Missing scope in", deparse(substitute(fn))))
    expect_true("execute" %in% names(args),
      info = paste("Missing execute in", deparse(substitute(fn))))
  }
})
