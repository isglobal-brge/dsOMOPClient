# ==============================================================================
# Test: Studio UI Sidebar Module
# Verifies sidebar module UI structure and defaults
# ==============================================================================

test_that("sidebar UI uses 280px width", {
  ui <- .mod_sidebar_ui("sidebar")
  html <- paste(as.character(ui), collapse = " ")

  # The sidebar width should be 280
  expect_true(grepl("280", html))
})

test_that("sidebar UI contains all accordion panels", {
  ui <- .mod_sidebar_ui("sidebar")
  html <- paste(as.character(ui), collapse = " ")

  expect_true(grepl("Context", html))
  expect_true(grepl("Cart", html))
})

test_that("sidebar UI contains scope controls", {
  ui <- .mod_sidebar_ui("sidebar")
  html <- paste(as.character(ui), collapse = " ")

  expect_true(grepl("Data Scope", html))
  expect_true(grepl("All Servers", html))
  expect_true(grepl("Per Site", html))
  expect_true(grepl("Pooled", html))
})

test_that("sidebar UI contains version badge", {
  ui <- .mod_sidebar_ui("sidebar")
  html <- paste(as.character(ui), collapse = " ")

  expect_true(grepl("dsOMOPClient", html))
})
