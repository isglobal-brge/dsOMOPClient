# Phase 4 (client): ds.omop.keepalive has been REMOVED.
#
# The server-side OMOP database connection now self-heals (transparent one-shot
# reconnect), so the client no longer needs a keepalive ping to keep the
# server-side DB connection warm. The function was removed outright (no shim:
# install is the update channel and nothing depended on it). These tests pin
# that contract and confirm the session machinery still works without it.

# --- keepalive is gone --------------------------------------------------------

test_that("ds.omop.keepalive is not exported", {
  ns <- asNamespace("dsOMOPClient")
  exports <- getNamespaceExports("dsOMOPClient")
  expect_false("ds.omop.keepalive" %in% exports)
})

test_that("ds.omop.keepalive is not defined anywhere in the namespace", {
  # Neither as an exported symbol nor as an internal one.
  ns <- asNamespace("dsOMOPClient")
  expect_false(exists("ds.omop.keepalive", envir = ns, inherits = FALSE))
})

test_that("calling dsOMOPClient::ds.omop.keepalive errors (object not found)", {
  expect_error(
    getExportedValue("dsOMOPClient", "ds.omop.keepalive"),
    "not an exported object"
  )
})

test_that("no man page remains for ds.omop.keepalive", {
  # The .Rd was deleted alongside the function.
  rd <- testthat::test_path("..", "..", "man", "ds.omop.keepalive.Rd")
  expect_false(file.exists(rd))
})

# --- the session still works without it ---------------------------------------

test_that("the public session API is intact without keepalive", {
  for (fn in c("ds.omop.connect", "ds.omop.disconnect", "ds.omop.status",
               "ds.omop.disclosure.settings")) {
    expect_true(exists(fn, envir = asNamespace("dsOMOPClient")),
                info = fn)
    expect_true(is.function(get(fn, envir = asNamespace("dsOMOPClient"))),
                info = fn)
  }
})

test_that("ds.omop.status (the documented manual connectivity probe) still exists", {
  # connect's @details now points users here instead of keepalive.
  st <- get("ds.omop.status", envir = asNamespace("dsOMOPClient"))
  expect_true(is.function(st))
  expect_true("symbol" %in% names(formals(st)))
})

test_that("session resource-map resolution is unaffected by the removal", {
  # A representative non-UI session helper still behaves correctly.
  m <- dsOMOPClient:::.resolve_resource_map(
    "proj.omop", c("nairobi", "douala", "dakar"))
  expect_equal(names(m), c("nairobi", "douala", "dakar"))
  expect_true(all(unlist(m) == "proj.omop"))
})
