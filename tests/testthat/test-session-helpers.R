# Tests for non-UI session helpers: per-server resource mapping.

# --- .resolve_resource_map -------------------------------------------------

test_that("a single resource string is applied to every server", {
  m <- .resolve_resource_map("proj.omop", c("nairobi", "douala", "dakar"))
  expect_equal(names(m), c("nairobi", "douala", "dakar"))
  expect_true(all(unlist(m) == "proj.omop"))
})

test_that("a named list maps each server to its own resource", {
  m <- .resolve_resource_map(
    list(nairobi = "a.mimic", douala = "b.omop", dakar = "c.cdm"),
    c("nairobi", "douala", "dakar"))
  expect_equal(m$nairobi, "a.mimic")
  expect_equal(m$douala, "b.omop")
  expect_equal(m$dakar, "c.cdm")
})

test_that("a NAMED VECTOR maps by name, not position (regression)", {
  # Order intentionally differs from server order.
  m <- .resolve_resource_map(c(dakar = "c", nairobi = "a", douala = "b"),
                             c("nairobi", "douala", "dakar"))
  expect_equal(m$nairobi, "a")
  expect_equal(m$douala, "b")
  expect_equal(m$dakar, "c")
})

test_that("an unnamed vector matches positionally to the servers", {
  m <- .resolve_resource_map(c("a", "b"), c("nairobi", "douala"))
  expect_equal(m$nairobi, "a")
  expect_equal(m$douala, "b")
})

test_that("strict mode errors on unknown or missing server names", {
  expect_error(
    .resolve_resource_map(list(mars = "x", nairobi = "a"),
                          c("nairobi", "douala"), strict = TRUE),
    "not among connected servers")
  expect_error(
    .resolve_resource_map(list(nairobi = "a"),
                          c("nairobi", "douala"), strict = TRUE),
    "no resource specified")
  expect_error(
    .resolve_resource_map(c("a", "b", "c"), c("nairobi", "douala")),
    "entries but there are")
})
