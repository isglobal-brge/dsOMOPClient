# WS-9f B1/B2: transport acknowledgements may be lost after a server writes.
# This backend tracks values as well as inventories so rollback assertions
# prove the original survived, rather than merely checking symbol names.
.tx_backend <- function(env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$values <- list(s1 = list(D = "original", E = "unrelated"),
                       s2 = list(D = "original", E = "unrelated"))
  state$conns <- list(s1 = "s1", s2 = "s2")
  state$backup <- NULL
  state$mutations <- character()
  state$restores <- character()
  state$removed <- character()
  state$phase <- "preflight"
  state$lose_response <- character()
  state$refuse <- character()
  state$restore_error <- character()
  state$restore_transport <- character()
  state$backup_transport <- character()
  state$interrupt <- FALSE
  state$restore_unacknowledged <- character()
  state$restore_missing <- character()
  state$inventory_error <- character()
  state$inventory_missing <- character()
  state$remove_error <- character()
  state$records <- character()
  testthat::local_mocked_bindings(
    .get_session = function(...) list(conns = state$conns, last_output = "D",
      capabilities = lapply(state$conns, function(s) list(recipe_language =
        list(version = 2L, operations = list("verb.mutate", "verb.filter_expr"))))),
    .dsomop_published_methods = function(conns, ...) .current_published_methods(conns),
    .record_session_owned_symbol = function(session_symbol, symbol) {
      state$records <- c(state$records, symbol)
    }, .package = "dsOMOPClient", .env = env)
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) {
      if (state$phase %in% state$inventory_error) {
        state$inventory_error <- setdiff(state$inventory_error, state$phase)
        stop(paste(state$phase, "inventory response lost"))
      }
      result <- lapply(state$values[names(conns)], names)
      if (state$phase %in% state$inventory_missing) {
        state$inventory_missing <- setdiff(state$inventory_missing, state$phase)
        result[[1L]] <- setdiff(result[[1L]], state$destination)
      }
      result
    },
    datashield.assign.expr = function(conns, symbol, expr, success = NULL, error = NULL, ...) {
      if (is.call(expr)) {
        state$phase <- "mutation"
        state$destination <- symbol
        for (site in names(conns)) {
          state$mutations <- c(state$mutations, site)
          if (site %in% state$refuse) {
            error(site, "server refused mutation")
            next
          }
          state$values[[site]][[symbol]] <- "mutated"
          if (isTRUE(state$interrupt)) stop(structure(
            list(message = "mutation interrupted", call = NULL), class = c("interrupt", "condition")))
          if (site %in% state$lose_response) stop("response lost after server commit")
          success(site)
        }
      } else {
        restoration <- identical(symbol, "D")
        state$phase <- if (restoration) "restoration" else "backup"
        if (!restoration) state$backup <- symbol
        for (site in names(conns)) {
          if (restoration) {
            state$restores <- c(state$restores, site)
            if (site %in% state$restore_error) {
              error(site, "restore refused")
              next
            }
          }
          state$values[[site]][[symbol]] <- state$values[[site]][[as.character(expr)]]
          if (restoration && site %in% state$restore_transport) stop("restore response lost")
          if (!restoration && site %in% state$backup_transport) stop("backup response lost")
          if (restoration && site %in% state$restore_missing) state$values[[site]][[symbol]] <- NULL
          if (restoration && site %in% state$restore_unacknowledged) next
          success(site)
        }
      }
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      state$phase <- "cleanup"
      for (site in names(conns)) {
        state$removed <- c(state$removed, paste(site, symbol, sep = ":"))
        if (site %in% state$remove_error) stop("remove transport failed")
        state$values[[site]][[symbol]] <- NULL
      }
      invisible(NULL)
    }, .package = "DSI", .env = env)
  state
}

.tx_in_place <- function(state) dsOMOPClient:::.assign_in_place(
  state$conns, "D", quote(omopMutateDS(D, "request")), "OMOP mutate")

test_that("lost mutation acknowledgements restore every possibly written site", {
  for (site in c("s1", "s2")) {
    state <- .tx_backend()
    state$lose_response <- site
    expect_error(.tx_in_place(state), "was left unchanged.*response lost")
    expect_identical(state$restores, c("s1", "s2"))
    expect_identical(state$values, list(s1 = list(D = "original", E = "unrelated"),
                                       s2 = list(D = "original", E = "unrelated")))
  }
})

test_that("failed or uncertain restoration retains that site's recoverable original", {
  for (failure in c("restore_error", "restore_unacknowledged", "restore_missing")) {
    state <- .tx_backend()
    state$refuse <- "s2"
    state[[failure]] <- "s1"
    message <- tryCatch(.tx_in_place(state), error = conditionMessage)
    expect_match(message, "could not be restored with confirmation on s1")
    expect_match(message, state$backup, fixed = TRUE)
    expect_false(grepl("was left unchanged", message, fixed = TRUE))
    expect_identical(state$values$s1[[state$backup]], "original")
    expect_false(state$backup %in% names(state$values$s2))
    expect_identical(state$values$s2$D, "original")
    expect_identical(state$restores, c("s1", "s2"))
  }
})

test_that("restoration inventory errors retain the original and do not stop other sites", {
  state <- .tx_backend()
  state$refuse <- "s2"
  state$inventory_error <- "restoration"
  expect_error(.tx_in_place(state), "could not be restored.*s1.*inventory response lost")
  expect_identical(state$values$s1[[state$backup]], "original")
  expect_false(state$backup %in% names(state$values$s2))
  expect_identical(state$restores, c("s1", "s2"))
})

test_that("in-place commit inventory errors and omissions enter rollback", {
  for (failure in c("inventory_error", "inventory_missing")) {
    state <- .tx_backend()
    state[[failure]] <- "mutation"
    expect_error(.tx_in_place(state), "was left unchanged")
    expect_identical(state$restores, c("s1", "s2"))
    expect_identical(state$values, list(s1 = list(D = "original", E = "unrelated"),
                                       s2 = list(D = "original", E = "unrelated")))
  }
})

test_that("backup disposal after confirmed commit reports committed state", {
  state <- .tx_backend()
  state$remove_error <- "s2"
  message <- tryCatch(.tx_in_place(state), error = conditionMessage)
  expect_match(message, "committed 'D' on every site, but backup cleanup failed")
  expect_match(message, "s2")
  expect_length(state$restores, 0L)
  expect_identical(state$values$s1$D, "mutated")
  expect_identical(state$values$s2$D, "mutated")
  expect_false(state$backup %in% names(state$values$s1))
  expect_identical(state$values$s2[[state$backup]], "original")
})

test_that("verified restoration and failed backup cleanup are reported independently", {
  state <- .tx_backend()
  state$refuse <- "s2"
  state$remove_error <- "s1"
  expect_error(.tx_in_place(state), "was left unchanged.*Backup cleanup failed.*s1")
  expect_identical(state$values$s1$D, "original")
  expect_identical(state$values$s2$D, "original")
  expect_identical(state$values$s1[[state$backup]], "original")
  expect_false(state$backup %in% names(state$values$s2))
})

test_that("fresh mutation and expression filter clean post-write verification failures", {
  requests <- list(
    function() ds.omop.mutate("D", y = 1L, newobj = "M"),
    function() ds.omop.filter("D", expr = omop_where(age >= 18), newobj = "M"))
  for (request in requests) for (failure in c("inventory_error", "inventory_missing")) {
    state <- .tx_backend()
    state[[failure]] <- "mutation"
    expect_error(request(), "rolled back.*commit")
    expect_identical(state$values, list(s1 = list(D = "original", E = "unrelated"),
                                       s2 = list(D = "original", E = "unrelated")))
    expect_setequal(state$removed, c("s1:M", "s2:M"))
    expect_length(state$records, 0L)
  }
})

test_that("fresh cleanup reports the original failure and every unresolved site", {
  state <- .tx_backend()
  state$inventory_error <- "mutation"
  state$remove_error <- "s1"
  message <- tryCatch(ds.omop.mutate("D", y = 1L, newobj = "M"), error = conditionMessage)
  expect_match(message, "inventory response lost.*cleanup could not be proven.*s1")
  expect_false(grepl("was rolled back", message, fixed = TRUE))
  expect_identical(state$values$s1$M, "mutated")
  expect_false("M" %in% names(state$values$s2))
  expect_identical(state$values$s1$D, "original")
  expect_identical(state$values$s2$D, "original")
  expect_setequal(state$removed, c("s1:M", "s2:M"))
  expect_length(state$records, 0L)
})

test_that("uncertain backup preparation cleans the fresh copy without replacing targets", {
  state <- .tx_backend()
  state$inventory_error <- "backup"
  expect_error(.tx_in_place(state), "rollback copy.*rolled back.*inventory response lost")
  expect_length(state$mutations, 0L)
  expect_identical(state$values, list(s1 = list(D = "original", E = "unrelated"),
                                     s2 = list(D = "original", E = "unrelated")))
})

test_that("fresh destination ownership is recorded only after verified commit", {
  state <- .tx_backend()
  expect_identical(ds.omop.mutate("D", y = 1L, newobj = "M"), "M")
  expect_identical(state$records, "M")
  expect_error(ds.omop.mutate("D", y = 1L, newobj = "E"), "already exists")
  expect_identical(state$values$s1$E, "unrelated")
  expect_identical(state$values$s2$E, "unrelated")
})


test_that("a lost restoration response retains the original even after the copy wrote", {
  state <- .tx_backend()
  state$lose_response <- "s2"
  state$restore_transport <- "s1"
  expect_error(.tx_in_place(state), "could not be restored.*s1.*restore response lost")
  expect_identical(state$values$s1$D, "original")
  expect_identical(state$values$s1[[state$backup]], "original")
  expect_false(state$backup %in% names(state$values$s2))
  expect_identical(state$restores, c("s1", "s2"))
})

test_that("a lost backup response cleans every copy before any replacement", {
  state <- .tx_backend()
  state$backup_transport <- "s2"
  expect_error(.tx_in_place(state), "rollback copy.*rolled back.*backup response lost")
  expect_length(state$mutations, 0L)
  expect_identical(state$values, list(s1 = list(D = "original", E = "unrelated"),
                                     s2 = list(D = "original", E = "unrelated")))
})

test_that("uncertain cleanup is reported even when removal probably succeeded", {
  state <- .tx_backend()
  state$inventory_error <- c("mutation", "cleanup")
  message <- tryCatch(ds.omop.mutate("D", y = 1L, newobj = "M"), error = conditionMessage)
  expect_match(message, "mutation inventory response lost.*cleanup could not be proven.*s1")
  expect_match(message, "cleanup inventory response lost")
  expect_false(grepl("was rolled back", message, fixed = TRUE))
  expect_false("M" %in% names(state$values$s1))
  expect_false("M" %in% names(state$values$s2))
  expect_length(state$records, 0L)
})

test_that("an interrupted replacement also restores all possibly written sites", {
  state <- .tx_backend()
  state$interrupt <- TRUE
  expect_error(.tx_in_place(state), "was left unchanged.*mutation interrupted")
  expect_identical(state$restores, c("s1", "s2"))
  expect_identical(state$values, list(s1 = list(D = "original", E = "unrelated"),
                                     s2 = list(D = "original", E = "unrelated")))
})
