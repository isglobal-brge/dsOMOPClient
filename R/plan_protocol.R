# Module: which execution protocol a federation supports (design-2.10.3
# sections 4.4 and 7.3; review finding B1).
#
# The publication protocol (the commit_policy argument of the execution
# methods, omopPlanStatusDS and omopStagedRevokeDS) arrived in one dsOMOP
# release, so the methods a site's profile publishes identify it. Before
# anything is assigned, every participating site is checked:
#   * every site supports the protocol: the protocol runs;
#   * a site lacks it and the plan is language 2: refused before any
#     assignment, naming the old sites, the missing methods and the upgrade;
#   * a site lacks it and the plan is a v1 plan: every site of the federation
#     receives the legacy call exactly as dsOMOPClient 2.10.2 sent it (same v1
#     payload, no commit_policy, no status or revocation calls), with one
#     warning that the cross-site rollback and section 7.3 guarantees do not
#     apply. The new guarantees are never emulated with old methods; each
#     server applies its own disclosure rules.

.DSOMOP_PROTOCOL_METHODS <- c("omopPlanStatusDS", "omopStagedRevokeDS")

#' Which execution protocol the participating sites support
#'
#' @param conns DSI connections of the execution.
#' @param server_method The assign method the execution calls
#'   (\code{omopPlanExecuteDS} or \code{omopDpPlanExecuteDS}).
#' @param harmonizes Whether the execution harmonises concept factors
#'   afterwards (\code{omopFactorLevelsDS}, \code{omopAsFactorColumnsDS}).
#' @return \code{list(protocol, old, missing)}: \code{protocol} is TRUE when
#'   every site publishes the publication protocol; \code{old} names the sites
#'   that do not and \code{missing} their missing protocol methods. A site
#'   whose profile cannot be listed, or that lacks a method the call needs on
#'   any route, stops the execution before anything is assigned.
#' @keywords internal
.plan_protocol_route <- function(conns, server_method = "omopPlanExecuteDS",
                                 harmonizes = FALSE) {
  published <- .dsomop_published_methods(conns)
  unlisted <- names(published)[vapply(published, function(x) !is.null(x$error), logical(1))]
  if (length(unlisted)) {
    stop("Cannot check the DataSHIELD methods published on ",
         paste(unlisted, collapse = ", "), " (",
         paste(unique(vapply(published[unlisted], `[[`, "", "error")), collapse = "; "),
         "); nothing was sent to any site.", call. = FALSE)
  }
  need_assign <- c(server_method, if (harmonizes) "omopAsFactorColumnsDS")
  need_aggregate <- if (harmonizes) "omopFactorLevelsDS" else character(0)
  lacking <- lapply(published, function(x) {
    c(setdiff(need_assign, x$assign), setdiff(need_aggregate, x$aggregate))
  })
  lacking <- lacking[lengths(lacking) > 0L]
  if (length(lacking)) {
    detail <- paste(vapply(names(lacking), function(s) {
      paste0(s, " lacks ", paste(lacking[[s]], collapse = ", "))
    }, ""), collapse = "; ")
    stop("The servers' DataSHIELD profiles do not publish the methods this execution ",
         "calls (", detail, "); nothing was sent to any site. ", .DSOMOP_PUBLISH_ADVICE,
         call. = FALSE)
  }
  missing <- lapply(published, function(x) setdiff(.DSOMOP_PROTOCOL_METHODS, x$aggregate))
  missing <- missing[lengths(missing) > 0L]
  list(protocol = length(missing) == 0L, old = names(missing), missing = missing)
}

.plan_protocol_detail <- function(route) {
  paste(vapply(route$old, function(s) {
    paste0(s, " lacks ", paste(route$missing[[s]], collapse = ", "))
  }, ""), collapse = "; ")
}

#' Refusal of an execution that needs the publication protocol on old sites
#' @param route A \code{.plan_protocol_route()} result.
#' @param what What needs the protocol.
#' @param v1_refusal Why a newly permitted shape has no v1 plan, or NULL.
#' @return A character message.
#' @keywords internal
.plan_protocol_refusal <- function(route, what = "A language-2 plan", v1_refusal = NULL) {
  paste0(if (!is.null(v1_refusal)) paste0(v1_refusal, " "),
         what, " needs the publication protocol of dsOMOP ", .DSOMOP_PROTOCOL_RELEASE,
         " on every site, and ", .plan_protocol_detail(route), " (",
         if (length(route$old) > 1L) "they run" else "it runs",
         " an older dsOMOP, or its methods are not published). Nothing was sent to any ",
         "site. An administrator installs dsOMOP ", .DSOMOP_PROTOCOL_RELEASE,
         " on those servers and publishes its methods (Administration > DataSHIELD > ",
         "Packages > dsOMOP > Publish).")
}

.plan_legacy_warning <- function(route) {
  paste0("dsOMOP on ", paste(route$old, collapse = ", "), " predates the publication ",
         "protocol (", .plan_protocol_detail(route), "): this v1 plan runs as the legacy ",
         "call on every site, as dsOMOPClient 2.10.2 sent it. Cross-site rollback and the ",
         "publication guarantees of section 7.3 (publication status, on_small_site) do ",
         "not apply until those servers run dsOMOP ", .DSOMOP_PROTOCOL_RELEASE,
         " with its methods published; each server applies its own disclosure rules.")
}

#' Execute a v1 plan with the legacy call of dsOMOPClient 2.10.2
#'
#' The execution of dsOMOPClient 2.10.2 (tag 2.10.2), unchanged: the same v1
#' payload and arguments (no \code{commit_policy}), fresh destinations for every
#' reserved composite sibling, one assignment per site, removal of the status
#' symbol, the static output-component check with removal of every freshly
#' assigned output on failure, and the federation's factor harmonisation. No
#' publication status is read and nothing is revoked.
#' @param plan A v1 plan (after the staged-mode preparation).
#' @param out The caller's out mapping.
#' @param symbol OMOP session symbol.
#' @param session The OMOP session.
#' @param conns DSI connections.
#' @param output_mode \code{"memory"} or \code{"staged"}.
#' @param server_method Assign method.
#' @param harmonize_factors Logical.
#' @return Invisibly, the resolved out mapping.
#' @keywords internal
.plan_execute_legacy <- function(plan, out, symbol, session, conns, output_mode,
                                 server_method, harmonize_factors) {
  plan <- .prepare_plan_for_federation(plan, symbol, conns)
  contract <- .session_harmonization_for_connections(session, conns)
  if (!is.null(contract)) {
    plan <- .apply_plan_harmonization(plan, contract)
    .validate_plan_harmonization(plan, contract)
  }
  out <- .resolve_plan_out(
    plan, out,
    reserved_symbols = c(symbol, session$res_symbol)
  )
  # 2.10.2's .resolve_plan_out() ended with this composite-family check
  .plan_output_symbol_families(out)
  owned <- .plan_expected_output_symbols(plan, out)
  cleanup_family <- .plan_output_symbol_families(out)
  reserved_targets <- unique(unlist(cleanup_family, use.names = FALSE))

  scope_args <- NULL
  combine <- "union"
  if (!is.null(plan$scope) &&
      (!is.null(plan$scope$cohort) || length(plan$scope$tables) > 0)) {
    scope_args <- .analysis_scope_expr(cohort = plan$scope$cohort,
                                       tables = plan$scope$tables)
    combine <- plan$scope$combine %||% "union"
  }
  scope_tables <- as.character(unlist(
    plan$scope$tables %||% character(0), use.names = FALSE
  ))
  collisions <- intersect(reserved_targets, scope_tables)
  if (length(collisions) > 0L) {
    stop("Output symbols collide with execution-scope input symbol(s): ",
         paste(collisions, collapse = ", "), ". Choose different outputs.",
         call. = FALSE)
  }

  # A distributed overwrite cannot be rolled back if one node fails after old
  # objects have been removed: fresh destinations, proven federation-wide.
  preflight_inventory <- .plan_symbol_inventory(
    conns, "plan output reservation"
  )
  occupied <- unlist(lapply(names(preflight_inventory), function(server) {
    found <- intersect(reserved_targets, preflight_inventory[[server]])
    if (length(found) == 0L) character(0) else paste0(server, ":", found)
  }), use.names = FALSE)
  if (length(occupied) > 0L) {
    stop("Plan output destinations must be fresh on every server; occupied: ",
         paste(occupied, collapse = ", "),
         ". Choose different out names or explicitly remove obsolete objects.",
         call. = FALSE)
  }
  exec_symbol <- NULL
  for (attempt in seq_len(10L)) {
    candidate <- .generate_symbol("dsOexec")
    if (!candidate %in% reserved_targets &&
        all(!vapply(preflight_inventory, function(x) candidate %in% x,
                    logical(1)))) {
      exec_symbol <- candidate
      break
    }
  }
  if (is.null(exec_symbol)) {
    stop("Could not reserve a fresh plan execution status symbol.",
         call. = FALSE)
  }

  exec_args <- list(
    as.name(server_method),
    # Encode singleton mappings too, keeping nested c() out of the wire call.
    session$res_symbol, .ds_encode(plan), .ds_encode(as.list(out)),
    output_mode)
  if (!is.null(scope_args)) {
    exec_args <- c(exec_args, scope_args, list(combine = combine))
  }

  expected_servers <- names(conns)
  assign_success <- character(0)
  assign_errors <- character(0)
  assign_condition <- tryCatch({
    DSI::datashield.assign.expr(
      conns,
      symbol = exec_symbol,
      expr = as.call(exec_args),
      success = function(server) {
        assign_success <<- c(assign_success, server)
      },
      error = function(server, message) {
        assign_errors[[server]] <<- message
      }
    )
    NULL
  }, error = identity)

  tryCatch(
    DSI::datashield.rm(conns, exec_symbol),
    error = function(e) NULL
  )

  failed_servers <- unique(c(
    names(assign_errors), setdiff(expected_servers, assign_success)
  ))
  if (!is.null(assign_condition) || length(failed_servers) > 0L) {
    reasons <- if (length(assign_errors) > 0L) as.list(assign_errors) else
      tryCatch(DSI::datashield.errors(), error = function(e) NULL)
    reasons <- reasons[vapply(reasons, function(x) {
      is.character(x) && length(x) >= 1L && nzchar(x[[1L]])
    }, logical(1))]
    .plan_remove_output_symbols(conns, cleanup_family, verify = TRUE)
    detail <- if (length(reasons) > 0L) {
      paste(names(reasons), vapply(reasons, `[[`, "", 1L), sep = ": ",
            collapse = "; ")
    } else if (!is.null(assign_condition)) {
      conditionMessage(assign_condition)
    } else {
      paste(failed_servers, collapse = ", ")
    }
    stop("Plan execution failed or was incomplete; freshly assigned outputs ",
         "were removed. Affected server(s): ", detail, ".", call. = FALSE)
  }

  missing <- .plan_missing_output_symbols(conns, owned)
  if (length(missing) > 0L) {
    .plan_remove_output_symbols(conns, cleanup_family, verify = TRUE)
    stop("Plan execution did not materialize every requested output ",
         "component; freshly assigned outputs were removed: ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }

  if (isTRUE(harmonize_factors) && identical(output_mode, "memory") &&
        isTRUE(plan$options$factor_concepts %||% TRUE)) {
    .harmonizeConceptFactors(owned, conns)
  }
  .record_session_outputs(symbol, owned)
  invisible(out)
}
