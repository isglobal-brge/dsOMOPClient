# Server publication transport fixtures. No rows or person counts cross this API.
.publication_status <- function(out = c(data = "D"), status = "ok",
                                rejected = list(), unavailable = character()) {
  list(status = status,
       published = lapply(out, function(symbol) {
         list(data = list(symbol = unname(symbol), grain = "person",
                          n_columns = 2L))
       }), rejected = rejected, unavailable = unavailable)
}

.publication_rejection <- function(code = "insufficient_support") {
  list(code = code, message = "Publication refused by server policy.")
}

.publication_backend <- function(statuses, initial = NULL, retrieve_error = FALSE,
                                 assign_error = NULL, missing = NULL,
                                 revoke_error = FALSE, remove_error = FALSE) {
  sites <- names(statuses)
  state <- new.env(parent = emptyenv())
  state$symbols <- initial %||% stats::setNames(rep(list(character()), length(sites)), sites)
  state$calls <- list()
  state$policies <- character()
  state$exec_symbol <- NULL
  state$revoked <- character()
  conns <- stats::setNames(as.list(sites), sites)
  # Every site publishes the current method inventory (the publication
  # protocol); helper-legacy-servers.R covers dsOMOP 2.10.2 sites.
  testthat::local_mocked_bindings(
    .get_session = function(...) list(conns = conns, res_symbol = "resource",
      capabilities = setNames(lapply(sites, function(s) .caps()), sites)),
    .prepare_plan_for_federation = function(plan, ...) plan,
    .session_harmonization_for_connections = function(...) NULL,
    .record_session_outputs = function(...) invisible(NULL),
    .dsomop_published_methods = .current_published_methods,
    .package = "dsOMOPClient", .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state$symbols[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      state$exec_symbol <- symbol
      state$policies <- c(state$policies, as.list(expr)$commit_policy)
      for (site in names(conns)) {
        if (site %in% assign_error) {
          if (is.function(error)) error(site, "backend failed")
          next
        }
        published <- unlist(lapply(statuses[[site]]$published, function(root) {
          vapply(root, `[[`, "", "symbol")
        }), use.names = FALSE)
        state$symbols[[site]] <- union(state$symbols[[site]],
                                        c(symbol, setdiff(published, missing)))
        if (is.function(success)) success(site)
      }
      invisible(NULL)
    },
    datashield.aggregate = function(conns, expr, ...) {
      method <- as.character(expr[[1L]])
      state$calls[[length(state$calls) + 1L]] <- list(method = method,
                                                     sites = names(conns))
      if (method == "omopPlanStatusDS") {
        if (retrieve_error) stop("status transport failed")
        for (site in names(conns)) {
          stopifnot(state$exec_symbol %in% state$symbols[[site]])
        }
        return(statuses[names(conns)])
      }
      if (method == "omopStagedRevokeDS") {
        if (revoke_error) stop("manifest revocation failed")
        for (site in names(conns)) {
          owned <- unlist(lapply(statuses[[site]]$published, function(root) {
            vapply(root, `[[`, "", "symbol")
          }), use.names = FALSE)
          state$symbols[[site]] <- setdiff(state$symbols[[site]], owned)
          state$revoked <- union(state$revoked, site)
        }
        return(stats::setNames(rep(list(TRUE), length(conns)), names(conns)))
      }
      stop("Unexpected aggregate: ", method)
    },
    datashield.rm = function(conns, symbol, ...) {
      if (remove_error) stop("status removal failed")
      for (site in names(conns)) {
        state$symbols[[site]] <- setdiff(state$symbols[[site]], symbol)
      }
      invisible(NULL)
    },
    .package = "DSI", .env = parent.frame()
  )
  state
}
