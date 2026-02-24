test_that("omop.feature.boolean creates correct spec", {
  spec <- omop.feature.boolean(c(201820), name = "diabetes")
  expect_s3_class(spec, "omop_feature_spec")
  expect_equal(spec$type, "boolean")
  expect_equal(spec$concept_set, c(201820))
  expect_equal(spec$name, "diabetes")
})

test_that("omop.feature.count creates correct spec", {
  spec <- omop.feature.count(c(3004410), name = "hba1c_count")
  expect_equal(spec$type, "count")
  expect_equal(spec$concept_set, c(3004410))
})

test_that("omop.feature.latest_value creates correct spec", {
  spec <- omop.feature.latest_value(c(3004410), value_column = "value_as_number")
  expect_equal(spec$type, "latest_value")
  expect_equal(spec$value_column, "value_as_number")
})

test_that("omop.feature.first_value creates correct spec", {
  spec <- omop.feature.first_value(c(3004410))
  expect_equal(spec$type, "first_value")
  expect_equal(spec$value_column, "value_as_number")
})

test_that("omop.feature.time_since creates correct spec", {
  spec <- omop.feature.time_since(c(201820), reference_date = "2024-01-01", unit = "day")
  expect_equal(spec$type, "time_since")
  expect_equal(spec$reference_date, "2024-01-01")
  expect_equal(spec$unit, "day")
})

test_that("omop.feature.mean_value creates correct spec", {
  spec <- omop.feature.mean_value(c(3025315), name = "avg_weight")
  expect_equal(spec$type, "mean_value")
  expect_equal(spec$name, "avg_weight")
})

test_that("omop.feature.min_value creates correct spec", {
  spec <- omop.feature.min_value(c(3025315))
  expect_equal(spec$type, "min_value")
})

test_that("omop.feature.max_value creates correct spec", {
  spec <- omop.feature.max_value(c(3025315))
  expect_equal(spec$type, "max_value")
})

test_that("feature specs have omop_feature_spec class", {
  specs <- list(
    omop.feature.boolean(1),
    omop.feature.count(1),
    omop.feature.latest_value(1),
    omop.feature.first_value(1),
    omop.feature.time_since(1),
    omop.feature.mean_value(1),
    omop.feature.min_value(1),
    omop.feature.max_value(1)
  )
  for (s in specs) {
    expect_s3_class(s, "omop_feature_spec")
  }
})

test_that("feature specs work with concept set lists", {
  cs <- list(concepts = c(3004410, 3025315), include_descendants = TRUE)
  spec <- omop.feature.boolean(cs, name = "labs")
  expect_equal(spec$concept_set, cs)
})
