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

test_that("disconnect proves handle cleanup and symbol removal on every node", {
  symbol <- "disconnect_ok"
  conns <- list(a = "A", b = "B")
  assign(symbol, list(res_symbol = "dsO_res", conns = conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env)) {
    rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
  }, add = TRUE)
  state <- list(a = symbol, b = symbol)
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      expect_identical(as.character(expr[[1L]]), "omopCleanupDS")
      expect_true(isTRUE(expr$close))
      for (server in names(conns)) {
        state[[server]] <<- union(state[[server]], symbol)
        success(server)
      }
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], symbol)
      invisible(NULL)
    },
    datashield.symbols = function(conns, ...) state[names(conns)],
    .package = "DSI"
  )
  expect_true(ds.omop.disconnect(symbol, conns))
  expect_false(exists(symbol, envir = dsOMOPClient:::.dsomop_client_env))
  expect_length(state$a, 0L)
  expect_length(state$b, 0L)
})

test_that("disconnect rejects a subset or replacement connection object", {
  symbol <- "disconnect_exact_connections"
  conns <- list(a = new.env(parent = emptyenv()),
                b = new.env(parent = emptyenv()))
  assign(symbol, list(res_symbol = "dsO_res", conns = conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env)) {
    rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
  }, add = TRUE)

  expect_error(
    ds.omop.disconnect(symbol, conns["a"]),
    "exactly the servers stored"
  )
  replacement <- conns
  replacement$b <- new.env(parent = emptyenv())
  expect_error(
    ds.omop.disconnect(symbol, replacement),
    "different connection object.*b"
  )
  expect_true(exists(symbol, envir = dsOMOPClient:::.dsomop_client_env))
})

test_that("disconnect retains its registry when one node cannot close", {
  symbol <- "disconnect_retry"
  conns <- list(a = "A", b = "B")
  assign(symbol, list(res_symbol = "dsO_res", conns = conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env)) {
    rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
  }, add = TRUE)
  state <- list(a = symbol, b = symbol)
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      state$a <<- union(state$a, symbol)
      success("a")
      error("b", "close failed")
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], symbol)
      invisible(NULL)
    },
    datashield.symbols = function(conns, ...) state[names(conns)],
    .package = "DSI"
  )
  expect_error(ds.omop.disconnect(symbol, conns), "retained for retry")
  expect_true(exists(symbol, envir = dsOMOPClient:::.dsomop_client_env))
  expect_true(symbol %in% state$a)
  expect_true(symbol %in% state$b)
})

test_that("disconnect retry treats an already removed public symbol as success", {
  symbol <- "disconnect_partial_public_rm"
  conns <- list(a = "A", b = "B")
  assign(symbol, list(res_symbol = "dsO_res", conns = conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env)) {
    rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
  }, add = TRUE)
  state <- list(a = symbol, b = symbol)
  fail_b_once <- TRUE

  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      for (server in names(conns)) {
        state[[server]] <<- union(state[[server]], symbol)
        success(server)
      }
      invisible(NULL)
    },
    datashield.rm = function(conns, target, ...) {
      server <- names(conns)[[1L]]
      if (identical(target, symbol) && !target %in% state[[server]]) {
        stop("symbol does not exist", call. = FALSE)
      }
      if (identical(target, symbol) && identical(server, "b") && fail_b_once) {
        fail_b_once <<- FALSE
        stop("transient remove failure", call. = FALSE)
      }
      state[[server]] <<- setdiff(state[[server]], target)
      invisible(NULL)
    },
    datashield.symbols = function(conns, ...) state[names(conns)],
    .package = "DSI"
  )

  expect_error(ds.omop.disconnect(symbol, conns),
               "symbol removal on: b.*retained for retry")
  expect_false(symbol %in% state$a)
  expect_true(symbol %in% state$b)
  expect_true(exists(symbol, envir = dsOMOPClient:::.dsomop_client_env))

  expect_true(ds.omop.disconnect(symbol, conns))
  expect_length(state$a, 0L)
  expect_length(state$b, 0L)
  expect_false(exists(symbol, envir = dsOMOPClient:::.dsomop_client_env))
})
