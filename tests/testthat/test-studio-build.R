# ==============================================================================
# Tests for Studio recipe builder module
# ==============================================================================

test_that("recipe builder UI supports YAML import/export", {
  ui <- .mod_build_recipe_ui("builder")
  html <- paste(as.character(ui), collapse = " ")

  expect_true(grepl("YAML", html))
  expect_true(grepl(".yml", html, fixed = TRUE))
  expect_true(grepl(".yaml", html, fixed = TRUE))
})
