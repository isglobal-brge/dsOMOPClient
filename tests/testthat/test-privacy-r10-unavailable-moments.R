test_that("unavailable protected moments do not prevent nested histogram pooling", {
  parent <- list(owner = "root", family = "HM", strata_kind = "root",
    bounds = c(0, 10), cell_keys = c("low", "high"), components = list(
      list(stratum = "root", n = 100, h = c(40, 60), s = NA_real_,
           q = NA_real_)))
  child <- list(owner = "child", family = "HM", strata_kind = "public",
    bounds = c(0, 10), cell_keys = c("low", "high"), parent_basis = parent,
    components = list(
      list(stratum = "a", n = 50, h = c(-10, 60), s = NA_real_, q = NA_real_),
      list(stratum = "b", n = 50, h = c(50, 0), s = NA_real_, q = NA_real_)))
  frame <- data.frame(value = NA_real_)
  attr(frame, "dsomop_release") <- list(
    protocol = .DP_PROTECTED_RELEASE_PROTOCOL, entry = "nested-unavailable",
    protection = "realised_sticky", owners = list(child, parent))
  pooled <- .pool_protected_release_frames(list(site = frame))$result
  expect_true(all(is.na(pooled$mean)))
  expect_true(all(is.na(pooled$sd)))
  expect_equal(pooled$persons, c(50, 50, 100))
  cells <- attr(pooled, "cells")
  nested <- cells[cells$owner == "child", ]
  expect_equal(tapply(nested$count, nested$cell, sum), c(high = 60, low = 40),
               tolerance = 1e-9, ignore_attr = TRUE)
  expect_equal(tapply(nested$count, nested$stratum, sum), c(a = 50, b = 50),
               tolerance = 1e-9, ignore_attr = TRUE)
  expect_equal(cells$count[cells$owner == "root"], c(40, 60))
})
