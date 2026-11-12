# Public signed hierarchy fixtures: incompatible projections must never
# replace a complete parent/child display with independent view projections.
.hc_nodes <- function(keys, parent, N, E, NE, T, R) {
  data.frame(key = keys, parent = parent, N_s = N, E_s = E, NE_s = NE,
              T_s = T, R_s = R, stringsAsFactors = FALSE)
}
.hc_owner <- function(nodes, level, view = NULL) {
  if (is.null(view)) view <- stats::setNames(as.list(seq_len(nrow(nodes[[level]]))),
                                            nodes[[level]]$key)
  list(owner = "C6/test", family = "C6", hierarchy_nodes = nodes,
       hierarchy = list(version = "c6-tree-gender-age5-year-v1",
         display = "l2-transportation-dykstra-v1", horizon_days = 3650,
         recurrent_cap = 10L, view_level = level, view_of_nodes = view),
       components = lapply(names(view), function(k) list(stratum = k)))
}

test_that("C6 pooling preserves shared parent status time and recurrent totals", {
  root <- .hc_nodes("root", NA_character_, 100, 20, 80, 1000, 30)
  gender <- .hc_nodes(c("gender=1", "gender=2"), "root",
                      c(50, 50), c(-10, 30), c(60, 20),
                      c(-100, 1100), c(-5, 35))
  owner <- .hc_owner(list(root = root, gender = gender), "gender")
  out <- .dp_pool_c6_hierarchy(list(owner))
  expect_equal(out$persons, c(50, 50))
  expect_equal(out$events, c(0, 20))
  expect_equal(out$person_time, c(0, 1000))
  expect_equal(out$recurrent, c(0, 30))
  both <- .dp_pool_c6_hierarchy(list(owner, owner))
  expect_equal(both$events, 2 * out$events)
  expect_equal(sum(both$person_time), 2000)
  expect_equal(sum(both$recurrent), 60)
  # A coarsening selects sums of already projected nodes.
  coarse <- owner
  coarse$hierarchy$view_of_nodes <- list(all = 1:2)
  coarse$components <- list(list(stratum = "all"))
  total <- .dp_pool_c6_hierarchy(list(coarse))
  expect_equal(total$events, sum(out$events))
  expect_equal(total$person_time, sum(out$person_time))
  malformed <- coarse
  malformed$hierarchy$view_of_nodes <- list(all = c(1L, 1L))
  expect_error(.dp_pool_c6_hierarchy(list(malformed)), "public view mapping")
})

test_that("C6 crossed cells retain both released age and year margins", {
  root <- .hc_nodes("root", NA_character_, 100, 20, 80, 1000, 30)
  gender <- root; gender$key <- "gender=1"; gender$parent <- "root"
  age <- .hc_nodes(c("gender=1|age5=0", "gender=1|age5=5"), "gender=1",
                    c(40, 60), c(5, 15), c(35, 45), c(400, 600), c(8, 22))
  year <- age; year$key <- c("gender=1|year=2025", "gender=1|year=2026")
  keys <- as.vector(outer(age$key, c("year=2025", "year=2026"), paste, sep = "|"))
  cross <- .hc_nodes(keys, "gender=1", c(15, 25, 25, 35),
                      c(-5, 10, 10, 5), c(20, 15, 15, 30),
                      c(-100, 500, 500, 100), c(-2, 10, 10, 12))
  nodes <- list(root = root, gender = gender, age = age, year = year,
                age_year = cross)
  c <- .dp_pool_c6_hierarchy(list(.hc_owner(nodes, "age_year")))
  a <- .dp_pool_c6_hierarchy(list(.hc_owner(nodes, "age")))
  y <- .dp_pool_c6_hierarchy(list(.hc_owner(nodes, "year")))
  for (field in c("persons", "events", "person_time", "recurrent")) {
    m <- matrix(c[[field]], 2L)
    expect_equal(rowSums(m), a[[field]], tolerance = 1e-9)
    expect_equal(colSums(m), y[[field]], tolerance = 1e-9)
    expect_true(all(m >= 0))
  }
})

test_that("nested histogram pooling retains the root histogram margins", {
  child <- list(owner = "child", family = "H", strata_kind = "public",
                 cell_keys = c("x", "y"), components = list(
                   list(stratum = "a", n = 50, h = c(-10, 60)),
                   list(stratum = "b", n = 50, h = c(50, 0))))
  child$parent_basis <- list(owner = "root", family = "H", strata_kind = "root",
                              cell_keys = c("x", "y"), components = list(
                                list(stratum = "root", n = 100, h = c(40, 60))))
  frame <- data.frame(x = 1)
  attr(frame, "dsomop_release") <- list(protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
    entry = "nested-test", protection = "realised_sticky", owners = list(child))
  out <- .pool_protected_release_frames(list(site = frame))$result
  cells <- attr(out, "cells")
  expect_equal(tapply(cells$count, cells$cell, sum), c(x = 40, y = 60),
               tolerance = 1e-9, ignore_attr = TRUE)
  expect_equal(tapply(cells$count, cells$stratum, sum), c(a = 50, b = 50),
               tolerance = 1e-9, ignore_attr = TRUE)
})
