test_that("cohort creation rejects legacy numeric thresholds before dispatch", {
  expect_error(
    ds.omop.cohort.create(list(
      type = "measurement",
      concept_set = 3004410L,
      value_threshold = list(op = ">=", value = 6.5)
    )),
    "no longer executable"
  )
})
