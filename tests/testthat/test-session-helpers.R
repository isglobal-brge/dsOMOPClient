# Tests for non-UI session helpers: per-server resource mapping.

.safe_aggregate_method_inventory <- function(conns, type = "aggregate", ...) {
  do.call(rbind, lapply(names(conns), function(server) {
    data.frame(
      name = "omopPingDS", value = "dsOMOP::omopPingDS",
      type = "aggregate", class = "function", package = "dsOMOP",
      version = "2.0.0", server = server, stringsAsFactors = FALSE
    )
  }))
}

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

test_that("aggregate-method inventory blocks raw c/list methods and aliases", {
  conns <- list(a = "A", b = "B")
  inventory <- .safe_aggregate_method_inventory(conns)
  testthat::local_mocked_bindings(
    datashield.methods = function(conns, type = "aggregate", ...) inventory,
    .package = "DSI"
  )

  expect_invisible(dsOMOPClient:::.assert_safe_aggregate_methods(conns))

  inventory <- .safe_aggregate_method_inventory(conns)
  inventory$value <- NULL
  expect_error(
    dsOMOPClient:::.assert_safe_aggregate_methods(conns),
    "complete DSI contract"
  )
  inventory <- .safe_aggregate_method_inventory(conns)
  inventory$type[inventory$server == "b"] <- "assign"
  expect_error(
    dsOMOPClient:::.assert_safe_aggregate_methods(conns),
    "non-aggregate method rows"
  )
  inventory <- .safe_aggregate_method_inventory(conns)
  inventory$name[inventory$server == "b"] <- "c"
  expect_error(
    dsOMOPClient:::.assert_safe_aggregate_methods(conns),
    "b/dsOMOP/c"
  )

  inventory <- .safe_aggregate_method_inventory(conns)
  unsafe_row <- inventory[1L, , drop = FALSE]
  unsafe_row[c("name", "value", "package", "server")] <-
    list("list", "base::list", "unsafe", "b")
  inventory <- rbind(inventory, unsafe_row)
  expect_error(
    dsOMOPClient:::.assert_safe_aggregate_methods(conns),
    "Unsafe DataSHIELD AggregateMethods.*b/unsafe/list->base::list"
  )

  inventory <- .safe_aggregate_method_inventory(conns)
  unsafe_row <- inventory[1L, , drop = FALSE]
  unsafe_row[c("name", "value", "package", "server")] <-
    list("innocent_alias", " base ::: c () ", "unsafe", "a")
  inventory <- rbind(inventory, unsafe_row)
  expect_error(
    dsOMOPClient:::.assert_safe_aggregate_methods(conns),
    "a/unsafe/innocent_alias.*base ::: c"
  )

  for (target in c("c", "list", "base::`c`")) {
    inventory <- .safe_aggregate_method_inventory(conns)
    inventory$value[inventory$server == "b"] <- target
    inventory$name[inventory$server == "b"] <- "raw"
    expect_error(
      dsOMOPClient:::.assert_safe_aggregate_methods(conns),
      paste0("b/dsOMOP/raw->", target), fixed = TRUE
    )
  }
})

test_that("aggregate-method inventory must be verifiable on every server", {
  conns <- list(a = "A", b = "B")
  inventory <- .safe_aggregate_method_inventory(conns["a"])
  testthat::local_mocked_bindings(
    datashield.methods = function(conns, type = "aggregate", ...) inventory,
    .package = "DSI"
  )
  expect_error(
    dsOMOPClient:::.assert_safe_aggregate_methods(conns),
    "missing server\\(s\\): b"
  )

  inventory <- data.frame(method = "omopPingDS", server = c("a", "b"))
  expect_error(
    dsOMOPClient:::.assert_safe_aggregate_methods(conns),
    "complete DSI contract"
  )

  testthat::local_mocked_bindings(
    datashield.methods = function(...) stop("registry offline", call. = FALSE),
    .package = "DSI"
  )
  expect_error(
    dsOMOPClient:::.assert_safe_aggregate_methods(conns),
    "Could not verify.*registry offline"
  )
})

test_that("ds.omop.connect blocks unsafe aggregate aliases before assignment", {
  conns <- list(a = "A", b = "B")
  assigned <- FALSE
  symbols_queried <- FALSE
  inventory <- .safe_aggregate_method_inventory(conns)
  unsafe_row <- inventory[1L, , drop = FALSE]
  unsafe_row[c("name", "value", "package", "server")] <-
    list("raw_container", "base::list", "external", "b")
  inventory <- rbind(inventory, unsafe_row)
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) {
      symbols_queried <<- TRUE
      stats::setNames(rep(list(character(0)), length(conns)), names(conns))
    },
    datashield.aggregate = function(conns, expr, ...) {
      stats::setNames(as.list(rep(TRUE, length(conns))), names(conns))
    },
    datashield.methods = function(conns, type = "aggregate", ...) inventory,
    datashield.assign.resource = function(...) {
      assigned <<- TRUE
      invisible(NULL)
    },
    .package = "DSI"
  )

  expect_error(
    ds.omop.connect("project.omop", symbol = "unsafe_methods",
                    strict = FALSE, conns = conns),
    "Unsafe DataSHIELD AggregateMethods"
  )
  expect_false(assigned)
  expect_false(symbols_queried)
  expect_false(exists("unsafe_methods",
                      envir = dsOMOPClient:::.dsomop_client_env,
                      inherits = FALSE))
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

test_that("ds.omop.login logs out authenticated connections when connect fails", {
  logged_out <- NULL
  testthat::local_mocked_bindings(
    datashield.login = function(logins, ...) list(server1 = "FAKE_CONN"),
    datashield.logout = function(conns, ...) {
      logged_out <<- conns
      invisible(TRUE)
    },
    .package = "DSI"
  )
  testthat::local_mocked_bindings(
    ds.omop.connect = function(...) stop("connect exploded", call. = FALSE)
  )

  expect_error(
    ds.omop.login(url = "https://opal.example.org", resource = "proj.omop"),
    "connect exploded"
  )
  expect_identical(logged_out, list(server1 = "FAKE_CONN"))
})

test_that("ds.omop.login reports an unproven logout after connect failure", {
  testthat::local_mocked_bindings(
    datashield.login = function(logins, ...) list(server1 = "FAKE_CONN"),
    datashield.logout = function(conns, ...) {
      stop("logout exploded", call. = FALSE)
    },
    .package = "DSI"
  )
  testthat::local_mocked_bindings(
    ds.omop.connect = function(...) stop("connect exploded", call. = FALSE)
  )

  expect_error(
    ds.omop.login(url = "https://opal.example.org", resource = "proj.omop"),
    "could not be logged out.*connect exploded.*logout exploded"
  )
})

test_that("unsafe-method connect failure logs out ds.omop.login connections", {
  conns <- list(server1 = "FAKE_CONN")
  logged_out <- NULL
  inventory <- .safe_aggregate_method_inventory(conns)
  inventory$name <- "raw_alias"
  inventory$value <- "base::list"
  inventory$package <- "external"
  testthat::local_mocked_bindings(
    datashield.login = function(logins, ...) conns,
    datashield.logout = function(active, ...) {
      logged_out <<- active
      invisible(TRUE)
    },
    datashield.methods = function(active, type = "aggregate", ...) inventory,
    .package = "DSI"
  )

  expect_error(
    ds.omop.login(
      url = "https://opal.example.org", resource = "project.omop",
      symbol = "unsafe_login_methods", strict = FALSE
    ),
    "Unsafe DataSHIELD AggregateMethods"
  )
  expect_identical(logged_out, conns)
  expect_false(exists("unsafe_login_methods",
                      envir = dsOMOPClient:::.dsomop_client_env,
                      inherits = FALSE))
})

test_that("ds.omop.connect builds a literal init call and commits atomically", {
  symbol <- "connect_literal"
  conns <- list(site = "FAKE")
  state <- list(site = character(0))
  captured <- new.env(parent = emptyenv())
  on.exit(if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env)) {
    rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
  }, add = TRUE)

  testthat::local_mocked_bindings(
    datashield.methods = .safe_aggregate_method_inventory,
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.aggregate = function(conns, expr, ...) {
      servers <- names(conns)
      if (identical(as.character(expr[[1L]]), "omopPingDS")) {
        return(stats::setNames(as.list(rep(TRUE, length(servers))), servers))
      }
      stats::setNames(lapply(servers, function(x) list(disclosure = list())),
                      servers)
    },
    datashield.assign.resource = function(conns, symbol, resource,
                                          success = NULL, error = NULL, ...) {
      for (server in names(conns)) {
        state[[server]] <<- union(state[[server]], symbol)
        success(server)
      }
      invisible(NULL)
    },
    datashield.assign.expr = function(conns, symbol, expr,
                                      success = NULL, error = NULL, ...) {
      captured$init <- expr
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
    .package = "DSI"
  )

  payload <- "cdm', close=TRUE); omopCleanupDS('victim"
  session <- ds.omop.connect(
    resource = "project.omop", symbol = symbol, cdm_schema = payload,
    conns = conns
  )
  expect_s3_class(session, "omop_session")
  expect_true(is.call(captured$init))
  expect_identical(as.character(captured$init[[1L]]), "omopInitDS")
  expect_identical(captured$init$cdm_schema, payload)
  expect_false(any(vapply(as.list(captured$init)[-1L], is.call, logical(1))))
  expect_identical(state$site, symbol)
})

test_that("ds.omop.connect rolls back every node after partial init", {
  symbol <- "connect_partial_init"
  conns <- list(a = "A", b = "B")
  state <- list(a = character(0), b = character(0))
  closed <- character(0)

  testthat::local_mocked_bindings(
    datashield.methods = .safe_aggregate_method_inventory,
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.aggregate = function(conns, expr, ...) {
      stats::setNames(as.list(rep(TRUE, length(conns))), names(conns))
    },
    datashield.assign.resource = function(conns, symbol, resource,
                                          success = NULL, error = NULL, ...) {
      for (server in names(conns)) {
        state[[server]] <<- union(state[[server]], symbol)
        success(server)
      }
      invisible(NULL)
    },
    datashield.assign.expr = function(conns, symbol, expr,
                                      success = NULL, error = NULL, ...) {
      method <- as.character(expr[[1L]])
      if (identical(method, "omopInitDS")) {
        state$a <<- union(state$a, symbol)
        success("a")
        error("b", "blueprint failed")
      } else {
        expect_identical(method, "omopCleanupDS")
        for (server in names(conns)) {
          closed <<- union(closed, server)
          state[[server]] <<- union(state[[server]], symbol)
          success(server)
        }
      }
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )

  expect_error(
    ds.omop.connect("project.omop", symbol = symbol, conns = conns),
    "Failed to initialize every OMOP handle"
  )
  expect_identical(closed, "a")
  expect_length(state$a, 0L)
  expect_length(state$b, 0L)
  expect_false(exists(symbol, envir = dsOMOPClient:::.dsomop_client_env))
})

test_that("ds.omop.connect rolls back a partial resource assignment", {
  symbol <- "connect_partial_resource"
  conns <- list(a = "A", b = "B")
  state <- list(a = character(0), b = character(0))
  init_called <- FALSE

  testthat::local_mocked_bindings(
    datashield.methods = .safe_aggregate_method_inventory,
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.aggregate = function(conns, expr, ...) {
      stats::setNames(as.list(rep(TRUE, length(conns))), names(conns))
    },
    datashield.assign.resource = function(conns, symbol, resource,
                                          success = NULL, error = NULL, ...) {
      state$a <<- union(state$a, symbol)
      success("a")
      error("b", "resource denied")
      invisible(NULL)
    },
    datashield.assign.expr = function(...) {
      init_called <<- TRUE
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )

  expect_error(
    ds.omop.connect("project.omop", symbol = symbol, conns = conns),
    "Failed to assign every OMOP resource"
  )
  expect_false(init_called)
  expect_length(state$a, 0L)
  expect_length(state$b, 0L)
})

test_that("ds.omop.connect refuses local and remote symbol replacement", {
  conns <- list(site = "FAKE")
  symbol <- "connect_collision"
  assign(symbol, list(), envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env,
                     inherits = FALSE)) {
    rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
  }, add = TRUE)
  expect_error(ds.omop.connect("project.omop", symbol = symbol, conns = conns),
               "already active")
  rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)

  testthat::local_mocked_bindings(
    datashield.methods = .safe_aggregate_method_inventory,
    datashield.symbols = function(conns, ...) {
      stats::setNames(list(symbol), names(conns))
    },
    datashield.aggregate = function(conns, expr, ...) {
      stats::setNames(as.list(rep(TRUE, length(conns))), names(conns))
    },
    .package = "DSI"
  )
  expect_error(ds.omop.connect("project.omop", symbol = symbol, conns = conns),
               "already exists")
})

# --- ds.omop.cohort.create auto cohort_id (footgun d) ----------------------

test_that("ds.omop.cohort.create auto-assigns a non-colliding cohort id", {
  assign("omop", list(conns = list(s = "FAKE"), res_symbol = "dsO.x"),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  state <- list(s = character(0))
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      for (server in names(conns)) {
        state[[server]] <<- union(state[[server]], symbol)
        success(server)
      }
      invisible(TRUE)
    },
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
