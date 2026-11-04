# Protection labels of analysis results (dsOMOP server 2.9.0).

test_that("protection labels are collected per server and printed", {
  dp <- data.frame(bin = 1:2, n_persons = c(10, 4))
  attr(dp, "dsomop_protection") <- list(
    protection = "dp", epsilon = 0.1, sensitivity = 1,
    estimand = "mean duration in days of the person's records",
    note = "Differential privacy: every count carries random noise."
  )
  standard <- data.frame(bin = 1:2, n_persons = c(10, 5))
  attr(standard, "dsomop_protection") <- list(
    protection = "standard_fallback",
    estimand = "mean duration in days of the person's records",
    note = "Standard DataSHIELD disclosure controls."
  )
  old <- data.frame(n = 1)
  labels <- .analysis_protection_labels(list(a = dp, b = standard, c = old))
  expect_identical(names(labels), c("a", "b"))
  expect_identical(labels$a$protection, "dp")

  printed <- paste(capture.output(.print_protection(labels)), collapse = "\n")
  expect_match(printed, "a: differential privacy \\(epsilon 0.1")
  expect_match(printed, "b: standard controls \\(DP unavailable")
  expect_match(printed, "Estimand: mean duration")
  expect_match(printed, "Servers differ in protection")

  same <- paste(capture.output(.print_protection(labels["a"])), collapse = "\n")
  expect_match(same, "all servers: differential privacy")
  expect_identical(capture.output(.print_protection(list())), character(0))
})

test_that("mixed tools modes are named when federated metadata differ", {
  meta <- function(mode) list(mode = "aggregate",
                              tools = list(mode = mode, shape = "summary"))
  expect_error(
    .analysis_consistent_metadata(list(a = meta("auto"), b = meta("standard")),
                                  c("a", "b"), "analysis metadata"),
    "dsomop.tools.mode: a = auto, b = standard"
  )
  expect_error(
    .analysis_consistent_metadata(list(a = list(x = 1), b = list(x = 2)),
                                  c("a", "b"), "analysis metadata"),
    "Align dsOMOP and analysis-pack versions"
  )
})
