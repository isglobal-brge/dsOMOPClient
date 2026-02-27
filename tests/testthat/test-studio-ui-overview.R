# ==============================================================================
# Test: Studio UI Overview Module
# Verifies overview module UI renders correctly
# ==============================================================================

test_that("overview UI contains KPI value boxes", {
  ui <- .mod_overview_ui("overview")
  html <- as.character(ui)

  expect_true(grepl("Servers", html))
  expect_true(grepl("CDM Tables", html))
  expect_true(grepl("Total Persons", html))
  expect_true(grepl("Cart Items", html))
})

test_that("overview UI contains refresh button", {
  ui <- .mod_overview_ui("overview")
  html <- as.character(ui)

  expect_true(grepl("Refresh", html))
})

test_that("overview UI contains connected servers section", {
  ui <- .mod_overview_ui("overview")
  html <- as.character(ui)

  expect_true(grepl("Connected Servers", html))
})

test_that("overview UI uses responsive layout for KPIs", {
  ui <- .mod_overview_ui("overview")
  html <- as.character(ui)

  expect_true(grepl("200px", html))
})
