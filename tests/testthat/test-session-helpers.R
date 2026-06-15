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

# --- ds.omop.login (one-call connect) --------------------------------------

test_that("ds.omop.login has a one-liner signature", {
  args <- formals(ds.omop.login)
  expect_true(all(c("url", "user", "password", "resource", "server", "driver",
                    "symbol") %in% names(args)))
  expect_equal(args$driver, "OpalDriver")
  expect_equal(args$symbol, "omop")
})

test_that("ds.omop.login validates url and resource", {
  expect_error(ds.omop.login(resource = "p.omop"), "needs at least one server")
  expect_error(ds.omop.login(url = "https://x"), "needs a 'resource'")
})

test_that("ds.omop.login builds the login + delegates to ds.omop.connect", {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    datashield.login = function(logins, ...) {
      captured$logins <- logins
      list(server1 = "FAKE_CONN")
    },
    .package = "DSI")
  testthat::local_mocked_bindings(
    ds.omop.connect = function(resource, symbol, conns, ...) {
      captured$resource <- resource
      captured$conns <- conns
      structure(list(symbol = symbol), class = "omop_session")
    })

  out <- ds.omop.login(url = "https://opal.example.org", user = "u",
                       password = "p", resource = "proj.omop_cdm")
  # The builder produced a single-server login row carrying url + resource.
  expect_equal(nrow(captured$logins), 1L)
  expect_equal(captured$logins$server, "server1")
  expect_equal(captured$resource, "proj.omop_cdm")
  expect_equal(captured$conns, list(server1 = "FAKE_CONN"))
  # Returns both the connections and the session.
  expect_equal(out$conns, list(server1 = "FAKE_CONN"))
  expect_s3_class(out$session, "omop_session")
})

# --- ds.omop.cohort.create auto cohort_id (footgun d) ----------------------

test_that("ds.omop.cohort.create auto-assigns a non-colliding cohort id", {
  assign("omop", list(conns = list(s = "FAKE"), res_symbol = "dsO.x"),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, ...) invisible(TRUE),
    .package = "DSI")

  spec <- list(type = "condition", concept_set = c(201820L))
  h1 <- ds.omop.cohort.create(spec = spec)
  h2 <- ds.omop.cohort.create(spec = spec)
  # Two un-id'd cohorts no longer both land on "dsomop_cohort_0".
  expect_true(grepl("^dsomop_cohort_[0-9]+$", unclass(h1)[1]))
  expect_false(identical(unclass(h1)[1], "dsomop_cohort_0"))
  expect_false(identical(unclass(h1)[1], unclass(h2)[1]))
  # A supplied id is still honoured exactly.
  h3 <- ds.omop.cohort.create(spec = spec, cohort_id = 7)
  expect_equal(unclass(h3)[1], "dsomop_cohort_7")
})
