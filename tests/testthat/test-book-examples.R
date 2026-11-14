test_that("book chapters execute against the released synthetic CDM", {
  for (pkg in c("dsOMOP","DSLite","dsBase","dsBaseClient","knitr","RSQLite")) {
    skip_if_not_installed(pkg)
  }
  root <- normalizePath(test_path("..", ".."))
  skip_if_not(file.exists(file.path(root,"tools/book/precompute.R")))
  withr::local_dir(root)
  # Source the exact authoring/build path; it checks values, types, roster,
  # dates, native-plan selection and parity with the released VA recipe.
  expect_error(sys.source("tools/book/precompute.R", new.env(parent=globalenv())), NA)
})
