test_that("ds.omop.concept.set creates correct structure", {
  cs <- ds.omop.concept.set(
    concepts = c(201820, 255573),
    include_descendants = TRUE,
    exclude = c(316139)
  )
  expect_s3_class(cs, "omop_concept_set")
  expect_equal(cs$concepts, c(201820L, 255573L))
  expect_true(cs$include_descendants)
  expect_equal(cs$exclude, 316139L)
})

test_that("ds.omop.concept.set handles defaults", {
  cs <- ds.omop.concept.set(c(201820))
  expect_false(cs$include_descendants)
  expect_false(cs$include_mapped)
  expect_null(cs$exclude)
})

test_that("ds.omop.concept.set coerces to integer", {
  cs <- ds.omop.concept.set(c(201820.5, 255573.1))
  expect_true(all(is.integer(cs$concepts)))
})
