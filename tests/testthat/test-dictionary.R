test_that("session environment exists", {
  expect_true(is.environment(.dsomop_client_env))
})

test_that(".get_session errors without connection", {
  expect_error(.get_session("nonexistent"), "No OMOP session")
})

test_that(".generate_symbol creates unique symbols", {
  s1 <- .generate_symbol("test")
  s2 <- .generate_symbol("test")
  expect_true(startsWith(s1, "test."))
  expect_true(startsWith(s2, "test."))
  expect_true(nchar(s1) > 5)
})
