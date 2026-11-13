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
         display = "l2-transportation-dykstra-feasible-fallback-v2", horizon_days = 3650,
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

.hc_tte_payload <- function(kind = "first", mass = 10) {
  node <- function(key, parent) data.frame(key = key, parent = parent)
  nodes <- list(A = node(paste0("A", 1:3), NA_integer_),
                B = node(paste0("B", 1:2), c(2L, 2L)),
                C = node(paste0("C", 1:4), c(1L, 1L, 2L, 2L)))
  signed <- list(A = c(-4, mass + 10, -6), B = c(-5, mass + 15),
                   C = c(-7, 2, mass / 2 + 7, mass / 2 + 8))
  levels <- lapply(names(nodes), function(l) list(keys = nodes[[l]]$key,
                                                  signed = signed[[l]]))
  names(levels) <- names(nodes)
  owner <- list(owner = paste0("TTE/", kind), population = "test", family = "TTE",
    basis = "tte-signed-day-basis-v1", estimand_kind = kind, cap = 10L,
    allocation = c(N = .2, R = .2, A = .2, B = .2, C = .2),
    levels = names(nodes), hierarchy_nodes = nodes,
    display_projection = "tte-hierarchy-simplex-v1",
    view = list(day_offset = c(-Inf, -2:1, 2), upper = c(-2:2, Inf),
                level = c("A", rep("C", 4), "A"),
                nodes = as.list(c(1L, 1:4, 3L))),
    components = list(list(stratum = "root", n = if (kind == "first") mass else 5,
                            r = mass, levels = levels)))
  if (kind == "recurrent") {
    owner$display_projection <- "tte-hierarchy-presence-v1"
    # One complete scale plus its coarsening; the same person may occupy
    # several children, so these are presence coordinates, not histogram cells.
    owner$presence_basis <- list(version = "tte-interval-presence-v1",
      projection = "tte-presence-radial-v1",
      intervals = data.frame(key = paste0("P", 1:7),
        lower = c(-Inf, -2:1, 2, -2), upper = c(-2:2, Inf, 2),
        level = c("A", rep("C", 4), "A", "A")),
      nodes = as.list(c(1L, 1:4, 3L, 2L)), views = list(),
      contains = cbind(child = 2:5, parent = rep(7L, 4)),
      partitions = list(list(parent = 0L, children = 1:6),
        list(parent = 0L, children = c(1L, 7L, 6L)),
        list(parent = 7L, children = 2:5)))
    owner$presence_view <- 1:6
    owner$components[[1]]$p <- c(-5, -2, 10, -3, 9, 4, 10)
  }
  list(protocol = .DP_PROTECTED_RELEASE_PROTOCOL, entry = "char.time_to_event",
       protection = "realised_sticky", owners = list(owner))
}

test_that("event-time pooling projects the full first or recurrent hierarchy", {
  for (kind in c("first", "recurrent")) {
    mass <- if (kind == "first") 10 else 30
    p <- .hc_tte_payload(kind, mass)
    out <- .pool_protected_tte(list(site = p))$result
    expect_equal(out$num_events, c(0, 0, 0, mass / 2 - .5, mass / 2 + .5, 0))
    expect_equal(sum(out$num_events), mass)
    if (kind == "first") expect_equal(out$persons, out$num_events) else
      {
        expect_true(all(out$persons >= out$num_events / 10 - 1e-8))
        expect_true(all(out$persons <= pmin(5, out$num_events) + 1e-8))
      }
    both <- .pool_protected_tte(list(a = p, b = p))$result
    expect_equal(both$num_events, 2 * out$num_events)
    expect_equal(both$persons, 2 * out$persons)
    coarse <- p
    coarse$owners[[1]]$view <- list(day_offset = c(-Inf, -2, 2),
      upper = c(-2, 2, Inf), level = rep("A", 3), nodes = as.list(1:3))
    if (kind == "recurrent") coarse$owners[[1]]$presence_view <- c(1L, 7L, 6L)
    c <- .pool_protected_tte(list(a = coarse, b = coarse))$result
    expect_equal(c$num_events, c(both$num_events[1], sum(both$num_events[2:5]),
                                  both$num_events[6]))
    frame <- data.frame(day_offset = numeric(), num_events = numeric(),
                          persons = numeric())
    attr(frame, "dsomop_release") <- p
    pooled <- .pool_protected_release_frames(list(site = frame),
      columns = list(day_offset = "double", num_events = "double", persons = "double"))
    expect_equal(pooled$result, out)
  }
})

test_that("event-time pooling sums signed nodes before any projection", {
  a <- .hc_tte_payload()
  b <- a
  b$owners[[1]]$components[[1]]$levels$A$signed <- c(12, 2, -4)
  b$owners[[1]]$components[[1]]$levels$B$signed <- c(3, -1)
  b$owners[[1]]$components[[1]]$levels$C$signed <- c(1, 2, -2, 1)
  out <- .pool_protected_tte(list(a = a, b = b))$result
  expect_equal(out$num_events, c(3, 0, 0, 6.5, 10.5, 0))
  separately <- .pool_protected_tte(list(a = a))$result$num_events +
    .pool_protected_tte(list(b = b))$result$num_events
  expect_false(isTRUE(all.equal(out$num_events, separately)))
})

test_that("event-time pooling refuses incompatible and malformed public hierarchies", {
  p <- .hc_tte_payload()
  bad <- p; bad$owners[[1]]$display_projection <- NULL
  expect_error(.pool_protected_tte(list(site = bad)), "incompatible basis/view")
  bad <- p; bad$owners[[1]]$estimand_kind <- "recurrent"
  expect_error(.pool_protected_tte(list(a = p, b = bad)), "incompatible basis/view")
  bad <- p; bad$owners[[1]]$view$nodes[[3]] <- 1L
  expect_error(.pool_protected_tte(list(site = bad)), "repeats protected nodes")
  bad <- p; bad$owners[[1]]$view$nodes[[2]] <- 1.5
  expect_error(.pool_protected_tte(list(site = bad)), "view mapping")
  bad <- p; bad$owners[[1]]$view$nodes <- bad$owners[[1]]$view$nodes[-3]
  bad$owners[[1]]$view$level <- bad$owners[[1]]$view$level[-3]
  bad$owners[[1]]$view$day_offset <- bad$owners[[1]]$view$day_offset[-3]
  expect_error(.pool_protected_tte(list(site = bad)), "omits hierarchy children")
  bad <- p; bad$owners[[1]]$hierarchy_nodes$B$parent[1] <- 4L
  expect_error(.pool_protected_tte(list(site = bad)), "parent indices")
  bad <- p; bad$owners[[1]]$components[[1]]$levels$C$signed[1] <- NA_real_
  expect_error(.pool_protected_tte(list(site = bad)), "signed time-to-event")
})

test_that("recurrent presence pooling uses the complete signed vector before view selection", {
  a <- .hc_tte_payload("recurrent", 30)
  b <- a
  b$owners[[1]]$components[[1]]$n <- 6
  b$owners[[1]]$components[[1]]$r <- 35
  b$owners[[1]]$components[[1]]$levels$A$signed <- c(-1, 40, -4)
  b$owners[[1]]$components[[1]]$levels$B$signed <- c(0, 40)
  b$owners[[1]]$components[[1]]$levels$C$signed <- c(0, 0, 20, 20)
  b$owners[[1]]$components[[1]]$p <- c(3, 2, 1, 0, 4, -5, 2)
  out <- .pool_protected_tte(list(a = a, b = b))$result
  expect_equal(out$num_events, c(0, 0, 0, 32, 33, 0))
  # Combined signed P gives a feasible target: lower event/K bound for the
  # fourth coordinate, N upper bound for the fifth and the common parent.
  expect_equal(out$persons, c(0, 0, 0, 3.2, 11, 0))
  separate <- .pool_protected_tte(list(a = a))$result$persons +
    .pool_protected_tte(list(b = b))$result$persons
  expect_false(isTRUE(all.equal(out$persons, separate)))
  coarse <- function(p) {
    p$owners[[1]]$view <- list(day_offset = c(-Inf, -2, 2),
      upper = c(-2, 2, Inf), level = rep("A", 3), nodes = as.list(1:3))
    p$owners[[1]]$presence_view <- c(1L, 7L, 6L)
    p
  }
  c <- .pool_protected_tte(list(a = coarse(a), b = coarse(b)))$result
  expect_equal(c$persons, c(0, 11, 0))
  expect_lt(c$persons[[2]], sum(out$persons))
  bad <- b; bad$owners[[1]]$presence_basis$intervals$key[[1]] <- "other-basis"
  expect_error(.pool_protected_tte(list(a = a, b = bad)), "incompatible basis/view")
  bad <- b; bad$owners[[1]]$components[[1]]$p <- NULL
  expect_error(.pool_protected_tte(list(a = a, b = bad)), "signed recurrent presence")
})
