# ==============================================================================
# Tests for client exploration wrappers
# ==============================================================================

test_that("ds.omop.concept.drilldown builds correct aggregate call", {
  # Verify the function exists and has expected signature
  expect_true(is.function(ds.omop.concept.drilldown))
  args <- formals(ds.omop.concept.drilldown)
  expect_true("table" %in% names(args))
  expect_true("concept_id" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
})

test_that("ds.omop.concept.locate builds correct aggregate call", {
  # Verify the function exists and has expected signature
  expect_true(is.function(ds.omop.concept.locate))
  args <- formals(ds.omop.concept.locate)
  expect_true("concept_ids" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
})
