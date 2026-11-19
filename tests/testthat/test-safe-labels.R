# dsOMOP 2.10.9: labels every dsBase function carries on Opal (measured on a
# real Opal: spaces, accents and most punctuation break ds.table()). The
# client's copy must give exactly the server's labels.

.safe_cases <- c("Acute myocardial infarction", "Neumonía", "Müller-Straße", "Fracture of radius, closed",
                 "[0,10)", "85+", "HIV/AIDS", "a b", "a_b", "男", NA, "NA", "Café (au lait)", "  padded  ")
.safe_expected <- c("Acute_myocardial_infarction", "Neumonia", "Muller-Strasse", "Fracture_of_radius_closed",
                    "0_10", "85+", "HIV_AIDS", "a_b", "a_b_9", "level", NA, "NA", "Cafe_au_lait", "padded")

test_that("safe labels keep letters, digits, . _ - + and stay unique", {
  expect_identical(.safeLabels(.safe_cases), .safe_expected)
  expect_true(all(grepl("^[A-Za-z0-9._+-]+$", na.omit(.safeLabels(.safe_cases)))))
})

test_that("the client gives exactly the server's labels", {
  testthat::skip_if_not_installed("dsOMOP")
  expect_identical(.safeLabels(.safe_cases), dsOMOP:::.safeLabels(.safe_cases))
  expect_identical(.safeLabels(c("男", "x"), c("101", "102")), dsOMOP:::.safeLabels(c("男", "x"), c("101", "102")))
})

test_that("a shared column keeps its exact labels as names", {
  column <- list(ids = c("101", "102", "103"), labels = list("男", "Café", "Café"), levels = c("男", "Café"))
  safe <- .conceptSafeColumn(column)
  expect_identical(safe$levels, c("101", "Cafe"))
  expect_identical(unlist(safe$labels), c("101", "Cafe", "Cafe"))
  expect_identical(safe$names, c("男", "Café"))
})
