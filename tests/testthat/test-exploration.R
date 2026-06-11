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

test_that("ds.omop.value.quantiles accepts a concept_id and plumbs it through", {
  args <- formals(ds.omop.value.quantiles)
  expect_true("concept_id" %in% names(args))
  expect_null(args$concept_id)

  # NULL concept_id (default) is omitted from the reproducible call code
  res_all <- ds.omop.value.quantiles("measurement", "value_as_number",
                                      execute = FALSE)
  expect_false(grepl("concept_id", res_all$meta$call_code))

  # A scalar concept_id is plumbed into the call code
  res_one <- ds.omop.value.quantiles("measurement", "value_as_number",
                                     concept_id = 3004410, execute = FALSE)
  expect_s3_class(res_one, "dsomop_result")
  expect_true(grepl("concept_id = 3004410", res_one$meta$call_code))
})

test_that("ds.omop.concept.summary has expected signature", {
  expect_true(is.function(ds.omop.concept.summary))
  args <- formals(ds.omop.concept.summary)
  expect_true("table" %in% names(args))
  expect_true("concept_id" %in% names(args))
  expect_true("column" %in% names(args))
  expect_true("scope" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_null(args$column)
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
  # scope default is per_site (first level of the match.arg vector)
  expect_equal(eval(args$scope)[1], "per_site")
})

test_that("ds.omop.concept.summary dispatches numeric vs categorical by column", {
  # No live session: stub the concept-scoped wrappers to capture dispatch.
  # value_as_number -> NUMERIC (stats + quantiles, never min/max).
  # value_as_concept_id -> CATEGORICAL (value counts).
  calls <- list(stats = NULL, quantiles = NULL, counts = NULL)
  testthat::local_mocked_bindings(
    ds.omop.column.stats = function(table, column, concept_id = NULL, ...) {
      calls$stats <<- list(table = table, column = column,
                           concept_id = concept_id)
      structure(list(per_site = list()), class = c("dsomop_result", "list"))
    },
    ds.omop.value.quantiles = function(table, value_col, concept_id = NULL,
                                       ...) {
      calls$quantiles <<- list(table = table, column = value_col,
                               concept_id = concept_id)
      structure(list(per_site = list()), class = c("dsomop_result", "list"))
    },
    ds.omop.value.counts = function(table, column, concept_id = NULL, ...) {
      calls$counts <<- list(table = table, column = column,
                            concept_id = concept_id)
      structure(list(per_site = list()), class = c("dsomop_result", "list"))
    },
    .package = "dsOMOPClient"
  )

  num <- ds.omop.concept.summary("measurement", concept_id = 3004410,
                                 column = "value_as_number")
  expect_equal(num$table, "measurement")
  expect_equal(num$concept_id, 3004410)
  expect_null(num$categorical)
  expect_true(!is.null(num$numeric$value_as_number$stats))
  expect_true(!is.null(num$numeric$value_as_number$quantiles))
  # Both numeric stats and quantiles received the concept filter
  expect_equal(calls$stats$concept_id, 3004410)
  expect_equal(calls$quantiles$concept_id, 3004410)
  expect_null(calls$counts)

  cat <- ds.omop.concept.summary("observation", concept_id = 4058243,
                                 column = "value_as_concept_id")
  expect_null(cat$numeric)
  expect_true(!is.null(cat$categorical$value_as_concept_id))
  expect_equal(calls$counts$concept_id, 4058243)
})

test_that("ds.omop.concept.summary auto-picks value columns from ds.omop.columns", {
  cols_df <- data.frame(
    column_name = c("measurement_id", "person_id",
                    "value_as_number", "value_as_concept_id"),
    stringsAsFactors = FALSE)
  seen <- character(0)
  testthat::local_mocked_bindings(
    ds.omop.columns = function(table, symbol = "omop", conns = NULL) {
      list(server1 = cols_df)
    },
    ds.omop.column.stats = function(table, column, concept_id = NULL, ...) {
      seen <<- c(seen, paste0("stats:", column))
      structure(list(per_site = list()), class = c("dsomop_result", "list"))
    },
    ds.omop.value.quantiles = function(table, value_col, concept_id = NULL,
                                       ...) {
      seen <<- c(seen, paste0("quantiles:", value_col))
      structure(list(per_site = list()), class = c("dsomop_result", "list"))
    },
    ds.omop.value.counts = function(table, column, concept_id = NULL, ...) {
      seen <<- c(seen, paste0("counts:", column))
      structure(list(per_site = list()), class = c("dsomop_result", "list"))
    },
    .package = "dsOMOPClient"
  )

  out <- ds.omop.concept.summary("measurement", concept_id = 3004410)
  # value_as_number routed to numeric (stats + quantiles)
  expect_true("stats:value_as_number" %in% seen)
  expect_true("quantiles:value_as_number" %in% seen)
  # value_as_concept_id routed to categorical (counts)
  expect_true("counts:value_as_concept_id" %in% seen)
  # non-value columns are ignored
  expect_false(any(grepl("person_id|measurement_id", seen)))
  expect_true(!is.null(out$numeric$value_as_number))
  expect_true(!is.null(out$categorical$value_as_concept_id))
})
