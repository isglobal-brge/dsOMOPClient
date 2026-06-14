# ==============================================================================
# Phase 1 client filter fixes
#
#   G3 (crash fix): omop_filter()'s value_bin label branch did params$value$lower
#       on an atomic, crashing for a scalar `value`. It must now branch on
#       is.list(params$value): list -> "v bin [lo, hi)"; scalar/missing ->
#       "<var> = <value>" / "<var> = ?". The list path is unchanged.
#   G2 (new constructor): omop_filter_value_concept() builds a level="row" filter
#       whose leaf is the disclosure-safe IN-membership shape the server DSL
#       supports: list(var=<col>, op="in", value=<ids>).
#   G5 (time-scoping): window= on omop_filter_not_has_concept() and
#       omop_filter_missing_measurement() is stored in params$window and reaches
#       the compiled population filter spec; it stays NULL when omitted.
# ==============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- G3: value_bin label must not crash on a scalar value ---------------------

test_that("value_bin label does not crash for a scalar value", {
  # Regression: the old branch evaluated params$value$lower on an atomic, which
  # errors "$ operator is invalid for atomic vectors".
  f <- expect_no_error(
    omop_filter(type = "value_bin", level = "row",
                params = list(var = "value_as_number", value = 5))
  )
  expect_s3_class(f, "omop_filter")
  expect_equal(f$label, "value_as_number = 5")
})

test_that("value_bin label handles a missing value without crashing", {
  f <- expect_no_error(
    omop_filter(type = "value_bin", level = "row",
                params = list(var = "value_as_number"))
  )
  expect_equal(f$label, "value_as_number = ?")
})

test_that("value_bin label list path is unchanged", {
  f <- omop_filter(type = "value_bin", level = "row",
                   params = list(var = "value_as_number",
                                 value = list(lower = 7, upper = 10)))
  expect_equal(f$label, "value_as_number bin [7, 10)")
})

# --- G2: omop_filter_value_concept builds the IN-membership spec --------------

test_that("omop_filter_value_concept builds a row-level IN-membership filter", {
  f <- omop_filter_value_concept(c(45877994L, 45884084L),
                                 concept_name = "Positive")
  expect_s3_class(f, "omop_filter")
  expect_equal(f$type, "value_concept")
  expect_equal(f$level, "row")
  expect_equal(f$params$var, "value_as_concept_id")
  expect_equal(f$params$op, "in")
  expect_equal(f$params$value, c(45877994L, 45884084L))
})

test_that("omop_filter_value_concept compiles to the server IN leaf", {
  f <- omop_filter_value_concept(c(45877994L, 45884084L))
  leaf <- dsOMOPClient:::.filter_to_leaf(f)
  expect_equal(leaf$var, "value_as_concept_id")
  expect_equal(leaf$op, "in")
  expect_equal(leaf$value, c(45877994L, 45884084L))
})

test_that("omop_filter_value_concept honours a custom column and is allow-listed", {
  f <- omop_filter_value_concept(45877994L, column = "qualifier_concept_id")
  expect_equal(f$params$var, "qualifier_concept_id")
  expect_equal(f$params$value, 45877994L)
  # value_concept must be classified as an always-allowed (disclosure-safe) row
  # filter, not constrained or blocked.
  expect_equal(dsOMOPClient:::.classifyFilterClient("value_concept"), "allowed")
})

test_that("a value_concept row filter flows into a compiled recipe plan", {
  r <- omop_recipe()
  v <- omop_variable(
    name = "smoking", table = "observation", concept_id = 4275495,
    filters = list(omop_filter_value_concept(c(45877994L, 45884084L))))
  r <- dsOMOPClient:::recipe_add_variable(r, v)
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "obs", type = "long"))

  plan <- recipe_to_plan(r)
  ft <- plan$outputs[[1]]$filters$custom
  expect_false(is.null(ft))
  leaf <- if ("and" %in% names(ft)) ft$and[[1]] else ft
  expect_equal(leaf$op, "in")
  expect_equal(leaf$var, "value_as_concept_id")
  expect_equal(leaf$value, c(45877994L, 45884084L))
})

# --- G5: window= reaches the compiled population filter spec ------------------

test_that("window on not_has_concept is stored and reaches the compiled spec", {
  f <- omop_filter_not_has_concept(1124300L, table = "drug_exposure",
                                   window = list(start = -365, end = 0))
  expect_equal(f$params$window$start, -365)
  expect_equal(f$params$window$end, 0)

  node <- dsOMOPClient:::.compile_population_filter_node(f)
  expect_equal(node$type, "not_has_concept")
  expect_equal(node$params$window$start, -365)
  expect_equal(node$params$window$end, 0)
})

test_that("window on missing_measurement is stored and reaches the compiled spec", {
  f <- omop_filter_missing_measurement(3004410L,
                                       window = list(start = -180, end = 0))
  expect_equal(f$params$window$start, -180)
  expect_equal(f$params$window$end, 0)

  node <- dsOMOPClient:::.compile_population_filter_node(f)
  expect_equal(node$type, "missing_measurement")
  expect_equal(node$params$window$start, -180)
})

test_that("window stays NULL when omitted (no spurious time-scoping)", {
  f1 <- omop_filter_not_has_concept(1124300L, table = "drug_exposure")
  f2 <- omop_filter_missing_measurement(3004410L)
  expect_null(f1$params$window)
  expect_null(f2$params$window)
  # And the compiled node carries no window either.
  expect_null(dsOMOPClient:::.compile_population_filter_node(f1)$params$window)
  expect_null(dsOMOPClient:::.compile_population_filter_node(f2)$params$window)
})

test_that("recipe_to_code round-trips value_concept and window filters", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_not_has_concept(
    1124300L, table = "drug_exposure", window = list(start = -365, end = 0)))
  r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_missing_measurement(
    3004410L, window = list(start = -180, end = 0)))
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
    name = "smoking", table = "observation", concept_id = 4275495,
    filters = list(omop_filter_value_concept(45877994L))))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "obs", type = "long"))

  code <- recipe_to_code(r)
  expect_true(grepl("omop_filter_value_concept", code))
  expect_true(grepl("omop_filter_not_has_concept", code))
  expect_true(grepl("omop_filter_missing_measurement", code))
  expect_true(grepl("window", code))

  # The generated code must be valid and reconstruct the window + value leaf.
  r2 <- eval(parse(text = code))
  expect_s3_class(r2, "omop_recipe")
  nhc <- Filter(function(f) f$type == "not_has_concept", r2$filters)[[1]]
  expect_equal(nhc$params$window$start, -365)
  vc <- r2$variables[[1]]$filters[[1]]
  expect_equal(vc$type, "value_concept")
  expect_equal(vc$params$value, 45877994L)
})
