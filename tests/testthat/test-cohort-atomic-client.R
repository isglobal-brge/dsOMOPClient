atomic_cohort_session <- function(symbol, conns) {
  assign(
    symbol,
    structure(list(symbol = symbol, res_symbol = "dsO.resource", conns = conns),
              class = "omop_session"),
    envir = dsOMOPClient:::.dsomop_client_env
  )
}

remove_atomic_cohort_session <- function(symbol) {
  if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env,
             inherits = FALSE)) {
    rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
  }
}

test_that("persistent cohort creation is blocked across several servers", {
  symbol <- "cohort_persistent_federated"
  conns <- list(a = "A", b = "B")
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  expect_error(
    ds.omop.cohort.create(
      list(type = "condition", concept_set = 201826L),
      mode = "persistent", cohort_id = 7L,
      symbol = symbol
    ),
    "no distributed database commit"
  )
  expect_error(
    ds.omop.cohort.create(
      list(type = "condition", concept_set = 201826L),
      mode = "persistent", cohort_id = 7L,
      symbol = symbol, conns = conns["a"]
    ),
    "no distributed database commit"
  )
})

test_that("temporary cohort handles follow the final inclusion-criteria table", {
  symbol <- "cohort_create_inclusions"
  conns <- list(a = "A", b = "B")
  state <- list(a = character(0), b = character(0))
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      expect_identical(as.character(expr[[1L]]), "omopCohortCreateDS")
      for (server in names(conns)) {
        state[[server]] <<- union(state[[server]], symbol)
        success(server)
      }
      invisible(NULL)
    },
    .package = "DSI"
  )

  handle <- ds.omop.cohort.create(
    list(
      type = "condition", concept_set = 201826L,
      inclusion_criteria = list(list(), list())
    ),
    cohort_id = 40L, symbol = symbol
  )
  expect_s3_class(handle, "dsomop_cohort_handle")
  expect_identical(unclass(handle)[[1L]], "dsomop_cohort_40_ic2")
  expect_identical(attr(handle, "symbol"), ".cohort_40")
})

test_that("partial temporary cohort creation performs exact verified rollback", {
  symbol <- "cohort_create_partial"
  conns <- list(a = "A", b = "B")
  state <- list(a = character(0), b = character(0))
  tables <- list(a = character(0), b = character(0))
  cleanup_seen <- character(0)
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      method <- as.character(expr[[1L]])
      if (identical(method, "omopCohortCreateDS")) {
        state$a <<- union(state$a, symbol)
        tables$a <<- union(tables$a, "dsomop_cohort_41_ic2")
        success("a")
        error("b", "creation failed")
      } else {
        expect_identical(method, "omopCleanupDS")
        expect_identical(names(conns), "a")
        expect_true(expr$prefix %in% c(
          "dsomop_cohort_41", "dsomop_cohort_41_ic1",
          "dsomop_cohort_41_ic2"
        ))
        expect_true(isTRUE(expr$exact))
        expect_false(isTRUE(expr$close))
        cleanup_seen <<- c(cleanup_seen, expr$prefix)
        for (server in names(conns)) {
          tables[[server]] <<- setdiff(tables[[server]], expr$prefix)
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
    ds.omop.cohort.create(
      list(
        type = "condition", concept_set = 201826L,
        inclusion_criteria = list(list(), list())
      ),
      cohort_id = 41L, symbol = symbol
    ),
    "failed and was rolled back"
  )
  expect_length(state$a, 0L)
  expect_length(state$b, 0L)
  expect_length(tables$a, 0L)
  expect_length(tables$b, 0L)
  expect_setequal(cleanup_seen, c(
    "dsomop_cohort_41", "dsomop_cohort_41_ic1", "dsomop_cohort_41_ic2"
  ))
  expect_true(exists(symbol, envir = dsOMOPClient:::.dsomop_client_env,
                     inherits = FALSE))
})

test_that("an incomplete exact cleanup reports unproven table removal", {
  symbol <- "cohort_cleanup_unproven"
  conns <- list(a = "A", b = "B")
  state <- list(a = character(0), b = character(0))
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      method <- as.character(expr[[1L]])
      if (identical(method, "omopCohortCreateDS")) {
        state$a <<- union(state$a, symbol)
        success("a")
        error("b", "creation failed")
      } else {
        expect_identical(method, "omopCleanupDS")
        expect_identical(names(conns), "a")
        state$a <<- union(state$a, symbol)
        error("a", "exact drop failed")
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
    ds.omop.cohort.create(
      list(type = "condition", concept_set = 201826L),
      cohort_id = 42L, symbol = symbol
    ),
    "temporary table and workspace symbol could not be proven on: a.*retained"
  )
  expect_true(exists(symbol, envir = dsOMOPClient:::.dsomop_client_env,
                     inherits = FALSE))
})

test_that("rollback never drops a homonymous table on a callback-error node", {
  symbol <- "cohort_preserve_error_node"
  conns <- list(a = "A", b = "B")
  state <- list(a = character(0), b = character(0))
  tables <- list(a = character(0), b = "dsomop_cohort_43")
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      method <- as.character(expr[[1L]])
      if (identical(method, "omopCohortCreateDS")) {
        state$a <<- union(state$a, symbol)
        tables$a <<- union(tables$a, "dsomop_cohort_43")
        success("a")
        error("b", "temporary table name is already owned")
      } else {
        expect_identical(method, "omopCleanupDS")
        expect_identical(names(conns), "a")
        tables$a <<- setdiff(tables$a, expr$prefix)
        state$a <<- union(state$a, symbol)
        success("a")
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
    ds.omop.cohort.create(
      list(type = "condition", concept_set = 201826L),
      cohort_id = 43L, symbol = symbol
    ),
    "failed and was rolled back"
  )
  expect_length(tables$a, 0L)
  expect_identical(tables$b, "dsomop_cohort_43")
})

test_that("rollback treats missing callbacks as an unknown cleanup state", {
  symbol <- "cohort_unknown_rollback"
  conns <- list(a = "A", b = "B")
  state <- list(a = character(0), b = character(0))
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(...) {
      stop("transport failed before callbacks", call. = FALSE)
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )

  expect_error(
    ds.omop.cohort.create(
      list(type = "condition", concept_set = 201826L),
      cohort_id = 44L, symbol = symbol
    ),
    "could not be proven on: a, b.*disconnect it before continuing"
  )
})

test_that("cohort combination commits only after every callback and inventory", {
  symbol <- "cohort_combine_commit"
  conns <- list(a = "A", b = "B")
  state <- list(a = character(0), b = character(0))
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      expect_identical(as.character(expr[[1L]]), "omopCohortCombineDS")
      for (server in names(conns)) {
        state[[server]] <<- union(state[[server]], symbol)
        success(server)
      }
      invisible(NULL)
    },
    .package = "DSI"
  )

  handle <- ds.omop.cohort.combine(
    "union", "dsomop_cohort_1", "dsomop_cohort_2",
    new_name = "dsomop_cohort_combined_safe1", symbol = symbol
  )
  expect_s3_class(handle, "dsomop_cohort_handle")
  expect_identical(unclass(handle)[[1L]], "dsomop_cohort_combined_safe1")
  expect_identical(attr(handle, "symbol"),
                   ".dsomop_cohort_combined_safe1")
  expect_true(all(vapply(state, function(x) {
    ".dsomop_cohort_combined_safe1" %in% x
  }, logical(1))))
})

test_that("cohort from_table requires its source on every server", {
  symbol <- "cohort_from_table_source"
  conns <- list(a = "A", b = "B")
  state <- list(a = "D", b = character(0))
  dispatched <- FALSE
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(...) {
      dispatched <<- TRUE
      invisible(NULL)
    },
    .package = "DSI"
  )

  expect_error(
    ds.omop.cohort.from_table(
      "D", new_name = "dsomop_cohort_fromtbl_safe1", symbol = symbol
    ),
    "source symbol.*b:D"
  )
  expect_false(dispatched)
})

test_that("cohort from_table returns a handle only after federated commit", {
  symbol <- "cohort_from_table_commit"
  conns <- list(a = "A", b = "B")
  state <- list(a = "D", b = "D")
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      expect_identical(as.character(expr[[1L]]), "omopCohortFromTableDS")
      expect_identical(as.character(expr[[2L]]), "D")
      for (server in names(conns)) {
        state[[server]] <<- union(state[[server]], symbol)
        success(server)
      }
      invisible(NULL)
    },
    .package = "DSI"
  )

  handle <- ds.omop.cohort.from_table(
    "D", new_name = "dsomop_cohort_fromtbl_safe2", symbol = symbol
  )
  expect_s3_class(handle, "dsomop_cohort_handle")
  expect_identical(unclass(handle)[[1L]], "dsomop_cohort_fromtbl_safe2")
  expect_true(all(vapply(state, function(x) {
    ".dsomop_cohort_fromtbl_safe2" %in% x
  }, logical(1))))
})

test_that("cohort outputs never replace an occupied workspace symbol", {
  symbol <- "cohort_output_collision"
  conns <- list(a = "A", b = "B")
  output <- ".dsomop_cohort_combined_taken"
  state <- list(a = output, b = character(0))
  dispatched <- FALSE
  atomic_cohort_session(symbol, conns)
  on.exit(remove_atomic_cohort_session(symbol), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.assign.expr = function(...) {
      dispatched <<- TRUE
      invisible(NULL)
    },
    .package = "DSI"
  )

  expect_error(
    ds.omop.cohort.combine(
      "union", "dsomop_cohort_1", "dsomop_cohort_2",
      new_name = "dsomop_cohort_1", symbol = symbol
    ),
    "must differ from both input"
  )
  expect_error(
    ds.omop.cohort.combine(
      "union", "dsomop_cohort_1", "dsomop_cohort_2",
      new_name = "dsomop_cohort_combined_taken", symbol = symbol
    ),
    "already exists on: a"
  )
  expect_false(dispatched)
})
