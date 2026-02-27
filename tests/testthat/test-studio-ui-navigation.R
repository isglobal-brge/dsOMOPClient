# ==============================================================================
# Test: Studio UI Navigation
# Verifies all main tabs are accessible and rendered
# ==============================================================================

test_that("studio UI has all navigation tabs", {
  ui <- .studio_ui("omop")
  ui_html <- as.character(ui(NULL))

  # All nav panel titles should appear
  expect_true(grepl("Overview", ui_html))
  expect_true(grepl("Explore", ui_html))
  expect_true(grepl("Data Sources", ui_html))
  expect_true(grepl("Queries", ui_html))
  expect_true(grepl("Build &amp; Preview", ui_html))
  expect_true(grepl("Execute &amp; Session", ui_html))
})

test_that("studio UI contains dark mode toggle", {
  ui <- .studio_ui("omop")
  ui_html <- as.character(ui(NULL))

  expect_true(grepl("dark", ui_html, ignore.case = TRUE))
})

test_that("studio UI contains clipboard JS handler with fallback", {
  ui <- .studio_ui("omop")
  ui_html <- as.character(ui(NULL))

  # Should contain the fallbackCopy function
  expect_true(grepl("fallbackCopy", ui_html))
  # Should check for secure context
  expect_true(grepl("isSecureContext", ui_html))
})

test_that("studio UI navbar has brand element", {
  ui <- .studio_ui("omop")
  ui_html <- as.character(ui(NULL))

  expect_true(grepl("dsOMOP", ui_html))
  expect_true(grepl("Studio", ui_html))
})

test_that("plotly defaults helper produces valid layout", {
  skip_if_not_installed("plotly")

  p <- plotly::plot_ly(x = 1:5, y = 1:5, type = "bar")
  styled <- .plotly_defaults(p, title = "Test")

  expect_s3_class(styled, "plotly")
})

test_that("empty state UI helper produces correct structure", {
  ui <- .empty_state_ui("database", "No data", "Try refreshing.")

  html <- as.character(ui)
  expect_true(grepl("empty-state", html))
  expect_true(grepl("No data", html))
  expect_true(grepl("Try refreshing", html))
  expect_true(grepl("database", html))
})

test_that("studio_colors has 10 colors", {
  expect_length(.studio_colors, 10)
  # All should be valid hex colors
  expect_true(all(grepl("^#[0-9a-fA-F]{6}$", .studio_colors)))
})
