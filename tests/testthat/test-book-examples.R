test_that("book chapters execute against the A-29 synthetic CDMs", {
  for (pkg in c("dsOMOP","DSLite","dsBase","dsBaseClient","knitr","RSQLite")) {
    skip_if_not_installed(pkg)
  }
  root <- normalizePath(test_path("..", ".."))
  skip_if_not(file.exists(file.path(root,"tools/book/precompute.R")))
  data_dir <- Sys.getenv("BOOK_PRACTICE_DIR", file.path(root,"..","practice-data"))
  skip_if_not(all(file.exists(file.path(data_dir,c("gibleed.sqlite","numeric.sqlite")))),
              "Load the two public practice databases with tools/book/load-practice.R first")
  withr::local_dir(root)
  # Source the exact authoring/build path; it checks values, types, roster,
  # dates, native-plan selection and parity with the released VA recipe.
  expect_error(sys.source("tools/book/precompute.R", new.env(parent=globalenv())), NA)
})
