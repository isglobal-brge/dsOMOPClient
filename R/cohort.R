# Module: Cohort Operations
# Client-side wrappers for cohort creation, combination, listing, and retrieval.

.cohort_table_name <- function(x, label, pattern = NULL) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[A-Za-z][A-Za-z0-9_]*$", x)) {
    stop(label, " must be one safe server-side table name.", call. = FALSE)
  }
  if (!is.null(pattern) && !grepl(pattern, x)) {
    stop(label, " does not use the required reserved namespace.",
         call. = FALSE)
  }
  x
}

.cohort_random_table <- function(prefix, inventory, context) {
  alphabet <- c(letters, LETTERS, 0:9)
  for (attempt in seq_len(20L)) {
    suffix <- paste(sample(alphabet, 12L, replace = TRUE), collapse = "")
    candidate <- paste0(prefix, suffix)
    output_symbol <- paste0(".", candidate)
    if (all(!vapply(inventory, function(x) output_symbol %in% x,
                    logical(1)))) {
      return(candidate)
    }
  }
  stop("Could not reserve a collision-free cohort name during ", context, ".",
       call. = FALSE)
}

.cohort_auto_id <- function(inventory) {
  for (attempt in seq_len(20L)) {
    candidate <- sample.int(900000L, 1L) + 99999L
    output_symbol <- paste0(".cohort_", candidate)
    if (all(!vapply(inventory, function(x) output_symbol %in% x,
                    logical(1)))) {
      return(as.integer(candidate))
    }
  }
  stop("Could not reserve a collision-free cohort id.", call. = FALSE)
}

.cohort_rollback <- function(conns, res_symbol, output_symbol, table_names,
                             cleanup_symbols, cleanup_servers = names(conns),
                             unknown_servers = character(0)) {
  expected <- names(conns)
  cleanup_servers <- intersect(expected, unique(cleanup_servers))
  unknown_servers <- intersect(expected, unique(unknown_servers))
  cleanup_unproven <- unknown_servers
  cleanup_conditions <- character(0)
  for (i in if (length(cleanup_servers) > 0L) seq_along(table_names) else
       integer(0)) {
    succeeded <- character(0)
    failures <- character(0)
    condition <- tryCatch({
      DSI::datashield.assign.expr(
        conns[cleanup_servers],
        symbol = cleanup_symbols[[i]],
        expr = call("omopCleanupDS", res_symbol,
                    prefix = table_names[[i]], exact = TRUE, close = FALSE),
        success = function(server) {
          succeeded <<- c(succeeded, server)
        },
        error = function(server, message) {
          failures[[server]] <<- message
        }
      )
      NULL
    }, error = identity)
    unproven <- unique(c(names(failures),
                         setdiff(cleanup_servers, succeeded)))
    if (!is.null(condition)) {
      unproven <- cleanup_servers
      cleanup_conditions <- c(
        cleanup_conditions,
        paste0(table_names[[i]], ": ", conditionMessage(condition))
      )
    }
    cleanup_unproven <- union(cleanup_unproven, unproven)
  }

  for (server in expected) {
    for (target in c(output_symbol, cleanup_symbols)) {
      tryCatch(DSI::datashield.rm(conns[server], target),
               error = function(e) NULL)
    }
  }

  final_inventory <- tryCatch(
    .plan_symbol_inventory(conns, "cohort rollback verification"),
    error = identity
  )
  lingering <- character(0)
  if (inherits(final_inventory, "error")) {
    workspace_unproven <- expected
  } else {
    workspace_unproven <- names(final_inventory)[vapply(
      final_inventory,
      function(x) any(c(output_symbol, cleanup_symbols) %in% x),
      logical(1)
    )]
    lingering <- unlist(lapply(workspace_unproven, function(server) {
      found <- intersect(c(output_symbol, cleanup_symbols),
                         final_inventory[[server]])
      paste0(server, ":", found)
    }), use.names = FALSE)
  }

  list(
    proven = length(cleanup_unproven) == 0L &&
      length(workspace_unproven) == 0L,
    cleanup_unproven = cleanup_unproven,
    workspace_unproven = workspace_unproven,
    lingering = lingering,
    cleanup_error = if (length(cleanup_conditions) == 0L) NULL else
      paste(cleanup_conditions, collapse = "; "),
    inventory_error = if (inherits(final_inventory, "error")) {
      conditionMessage(final_inventory)
    } else NULL
  )
}

.cohort_stop_after_rollback <- function(context, detail, rollback,
                                        session_symbol) {
  if (isTRUE(rollback$proven)) {
    stop(context, " failed and was rolled back: ", detail, ".",
         call. = FALSE)
  }
  unproven <- unique(c(rollback$cleanup_unproven,
                       rollback$workspace_unproven))
  extra <- c(
    if (length(rollback$lingering) > 0L) {
      paste0(" lingering symbol(s): ",
             paste(rollback$lingering, collapse = ", "), ".")
    },
    if (!is.null(rollback$cleanup_error)) {
      paste0(" cleanup transport error: ", rollback$cleanup_error, ".")
    },
    if (!is.null(rollback$inventory_error)) {
      paste0(" inventory error: ", rollback$inventory_error, ".")
    }
  )
  stop(
    context, " failed; removal of its temporary table and workspace symbol ",
    "could not be proven on: ", paste(unproven, collapse = ", "), ". ",
    "The OMOP session '", session_symbol,
    "' was retained; disconnect it before continuing. Original error: ",
    detail, ".", paste0(extra, collapse = ""),
    call. = FALSE
  )
}

.cohort_assign_temporary <- function(conns, res_symbol, output_symbol,
                                     table_name, expr, context,
                                     session_symbol,
                                     required_symbols = character(0),
                                     rollback_tables = table_name) {
  inventory <- .plan_symbol_inventory(conns, paste0(context, " preflight"))
  occupied <- names(inventory)[vapply(
    inventory, function(x) output_symbol %in% x, logical(1)
  )]
  if (length(occupied) > 0L) {
    stop(context, " output symbol '", output_symbol, "' already exists on: ",
         paste(occupied, collapse = ", "), ". Choose a fresh cohort name/id.",
         call. = FALSE)
  }
  missing_sources <- unlist(lapply(names(inventory), function(server) {
    missing <- setdiff(required_symbols, inventory[[server]])
    if (length(missing) == 0L) character(0) else
      paste0(server, ":", missing)
  }), use.names = FALSE)
  if (length(missing_sources) > 0L) {
    stop(context, " source symbol(s) are unavailable: ",
         paste(missing_sources, collapse = ", "), ".", call. = FALSE)
  }
  rollback_tables <- unique(vapply(
    rollback_tables, .cohort_table_name, character(1), label = "rollback table"
  ))
  reservation_inventory <- inventory
  cleanup_symbols <- vapply(seq_along(rollback_tables), function(i) {
    candidate <- .fresh_symbol_from_inventory(
      reservation_inventory, "dsOcohortCleanup", paste0(context, " preflight")
    )
    reservation_inventory <<- lapply(
      reservation_inventory, union, y = candidate
    )
    candidate
  }, character(1))

  succeeded <- character(0)
  failures <- character(0)
  condition <- tryCatch({
    DSI::datashield.assign.expr(
      conns, symbol = output_symbol, expr = expr,
      success = function(server) {
        succeeded <<- c(succeeded, server)
      },
      error = function(server, message) {
        failures[[server]] <<- message
      }
    )
    NULL
  }, error = identity)
  incomplete <- unique(c(names(failures), setdiff(names(conns), succeeded)))
  if (!is.null(condition) || length(incomplete) > 0L) {
    detail <- if (!is.null(condition)) conditionMessage(condition) else
      paste0("incomplete federation (", paste(incomplete, collapse = ", "), ")")
    known <- unique(c(succeeded, names(failures)))
    unknown <- setdiff(names(conns), known)
    rollback <- .cohort_rollback(
      conns, res_symbol, output_symbol, rollback_tables, cleanup_symbols,
      cleanup_servers = succeeded, unknown_servers = unknown
    )
    .cohort_stop_after_rollback(context, detail, rollback, session_symbol)
  }

  committed <- tryCatch(
    .plan_symbol_inventory(conns, paste0(context, " commit")),
    error = identity
  )
  missing_output <- if (inherits(committed, "error")) {
    names(conns)
  } else {
    names(committed)[!vapply(
      committed, function(x) output_symbol %in% x, logical(1)
    )]
  }
  if (inherits(committed, "error") || length(missing_output) > 0L) {
    detail <- if (inherits(committed, "error")) {
      paste0("commit inventory failed: ", conditionMessage(committed))
    } else {
      paste0("commit was absent on ", paste(missing_output, collapse = ", "))
    }
    rollback <- .cohort_rollback(
      conns, res_symbol, output_symbol, rollback_tables, cleanup_symbols,
      cleanup_servers = names(conns)
    )
    .cohort_stop_after_rollback(context, detail, rollback, session_symbol)
  }

  .record_session_owned_symbol(session_symbol, output_symbol)
  invisible(output_symbol)
}

#' List available cohort definitions
#'
#' Queries each connected server for existing cohort definitions. Returns a
#' named list (one entry per server) of data frames describing the cohorts that
#' are usable for analysis. This is useful for discovering cohorts that have
#' already been created in persistent storage.
#'
#' @section Disclosure control:
#' Listing is gated server-side and is intentionally a partial, approximate view
#' so that discovery is itself disclosure-safe:
#' \itemize{
#'   \item Only cohorts whose distinct-subject count reaches the server's
#'     per-subset threshold (\code{nfilter_subset}) appear. A cohort below that
#'     threshold is OMITTED entirely -- it never shows up in the listing, exactly
#'     as if it did not exist. You therefore cannot use the listing to learn that
#'     a small cohort exists.
#'   \item Each surviving cohort's size is BANDED (rounded down to a multiple of
#'     \code{nfilter_band}), never the exact subject count, so the listing cannot
#'     be differenced to recover an individual's membership.
#' }
#' The net effect: you can discover the cohorts you can actually use and their
#' approximate size, but never tiny cohorts and never an exact count. The gating
#' and banding happen on the server through the shared disclosure helpers; the
#' client surfaces the server's response unchanged.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Named list (one entry per server) of data frames with cohort
#'   metadata. Each row is a cohort at or above the server's
#'   \code{nfilter_subset} threshold; its reported size is BANDED to a multiple
#'   of \code{nfilter_band} (never the exact subject count). Sub-threshold
#'   cohorts are intentionally absent. Returns an empty list (or per-server empty
#'   data frames) when no cohort clears the threshold.
#' @examples
#' \dontrun{
#' cohorts <- ds.omop.cohort.list()
#' cohorts[["server_a"]]
#' }
#' @export
ds.omop.cohort.list <- function(symbol = "omop",
                                conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  .ds_safe_aggregate(
    conns,
    expr = call("omopCohortListDS", session$res_symbol)
  )
}

#' Get a cohort definition by ID
#'
#' Retrieves the full definition of a specific cohort from each connected
#' server, including the inclusion criteria and any metadata stored with
#' the cohort definition.
#'
#' @section Disclosure control:
#' An id that is not available -- whether because no such cohort exists OR
#' because the cohort exists but is below the server's per-subset threshold
#' (\code{nfilter_subset}) -- yields the SAME "not available" result. The two
#' cases are deliberately indistinguishable: a sub-threshold cohort is treated as
#' nonexistent, and the server's response for it is identical to that for a
#' genuinely unknown id (it never says "too small" or otherwise hints that a
#' small cohort exists, which would itself confirm its existence). The client
#' surfaces the server's response as-is and adds no distinction of its own, so a
#' caller cannot tell small-but-present from absent from either side. Only
#' definitions of cohorts at or above the threshold (the ones that appear in
#' \code{\link{ds.omop.cohort.list}}) are readable.
#'
#' @param id Integer; the cohort definition ID to retrieve. An id that is unknown
#'   or below the disclosure threshold returns the same not-available result (see
#'   Disclosure control), so a usable definition requires a cohort that appears in
#'   \code{\link{ds.omop.cohort.list}}.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Named list (one entry per server) with definition details for an
#'   available (at- or above-threshold) cohort. For an unavailable id -- absent
#'   OR sub-threshold -- the server returns its uniform not-available response
#'   (identical for both cases), surfaced unchanged by the client.
#' @examples
#' \dontrun{
#' defn <- ds.omop.cohort.definition(id = 1)
#' defn[["server_a"]]
#' }
#' @seealso \code{\link{ds.omop.cohort.list}}
#' @export
ds.omop.cohort.definition <- function(id,
                                      symbol = "omop",
                                      conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  .ds_safe_aggregate(
    conns,
    expr = call("omopCohortGetDefinitionDS",
                session$res_symbol,
                as.integer(id))
  )
}

#' Create a cohort from a structured specification
#'
#' Creates a cohort definition on each connected server based on the provided
#' cohort specification. The cohort is stored as a temporary or persistent
#' table server-side and can be used to filter subsequent queries and plan
#' executions. The specification must include a \code{type} field and a
#' \code{concept_set} defining the clinical events that constitute cohort
#' entry.
#'
#' @section Disclosure control:
#' The resulting cohort is gated server-side on its distinct-subject count: if
#' the spec (including any \code{inclusion_criteria}) selects fewer than the
#' server's per-subset threshold (\code{nfilter_subset}) persons, creation FAILS
#' CLOSED with an "insufficient individuals" error and no table is materialised.
#' Because you authored the criteria, an explicit error here is expected and
#' carries no disclosure (it only reflects your own spec); contrast this with the
#' uniform, silent omission used for pre-existing small cohorts in
#' \code{\link{ds.omop.cohort.list}} / \code{\link{ds.omop.cohort.definition}}.
#'
#' @param spec Named list defining the cohort. Must contain at least
#'   \code{type} (character; one of \code{"condition"}, \code{"drug"},
#'   \code{"measurement"}, \code{"observation"}, \code{"procedure"}) and
#'   \code{concept_set} (integer vector or \code{omop_concept_set} object),
#'   and optionally an authenticated \code{value_bin} filter returned by
#'   \code{ds.omop.safe.filter.value()}.
#' @param mode Character; \code{"temporary"} (the default) creates a
#'   session-scoped temp table, \code{"persistent"} writes to the cohort
#'   schema for reuse across sessions. Persistent creation is restricted to one
#'   server because DataSHIELD cannot provide a distributed database commit.
#' @param cohort_id Integer; cohort definition ID. If \code{NULL}, an
#'   auto-generated ID is used.
#' @param name Character; human-readable cohort name for display purposes.
#'   Optional.
#' @param overwrite Logical; if \code{TRUE}, an existing cohort with the
#'   same \code{cohort_id} will be replaced. Default: \code{FALSE}.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Invisibly; a \code{dsomop_cohort_handle} object carrying the
#'   deterministic server-side cohort TABLE name (e.g.
#'   \code{"dsomop_cohort_1"}, or \code{"dsomop_cohort_1_ic2"} after two
#'   inclusion criteria) for a temporary cohort, or \code{NULL} for a persistent
#'   cohort. The table is assigned server-side via
#'   \code{DSI::datashield.assign.expr}. Pass the returned handle straight
#'   into \code{ds.omop.cohort.combine()}.
#' @examples
#' \dontrun{
#' diabetes <- ds.omop.cohort.create(
#'   spec = list(type = "condition",
#'               concept_set = c(201820, 201826)),
#'   cohort_id = 1,
#'   name = "Type 2 Diabetes"
#' )
#' # The returned handle feeds directly into ds.omop.cohort.combine():
#' # ds.omop.cohort.combine(op = "union", cohort_a = diabetes, cohort_b = ...)
#' }
#' @export
ds.omop.cohort.create <- function(spec,
                                  mode = "temporary",
                                  cohort_id = NULL,
                                  name = NULL,
                                  overwrite = FALSE,
                                  symbol = "omop",
                                  conns = NULL) {
  mode <- match.arg(mode, c("temporary", "persistent"))
  if (!is.list(spec)) {
    stop("Cohort spec must be a named list.", call. = FALSE)
  }
  if (!is.null(spec$value_threshold)) {
    stop("value_threshold is no longer executable; obtain a server-issued ",
         "value_bin with ds.omop.safe.filter.value().", call. = FALSE)
  }
  if (inherits(spec$value_bin, "omop_filter")) {
    if (!identical(spec$value_bin$type, "value_bin") ||
        is.null(spec$value_bin$params$value) ||
        is.null(spec$value_bin$params$safe_scope)) {
      stop("spec$value_bin must be an authenticated value_bin filter.",
           call. = FALSE)
    }
    spec$value_bin <- list(
      lower = spec$value_bin$params$value$lower,
      upper = spec$value_bin$params$value$upper,
      safe_scope = spec$value_bin$params$safe_scope
    )
  }
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  if (identical(mode, "persistent") &&
      (length(session$conns) > 1L || length(conns) > 1L)) {
    stop("Persistent cohort creation is limited to one server because no ",
         "distributed database commit is available; use mode='temporary' ",
         "for federated cohorts.", call. = FALSE)
  }

  if (is.null(spec$type)) {
    stop("Cohort spec must include 'type'.", call. = FALSE)
  }
  if (is.null(spec$concept_set) || length(spec$concept_set) == 0) {
    stop("Cohort spec must include 'concept_set'.",
         call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE.", call. = FALSE)
  }

  inventory <- .plan_symbol_inventory(conns, "cohort creation preflight")

  # Auto-assign a non-colliding cohort id when the caller supplies none, so two
  # un-id'd cohorts no longer both land on id 0 / table "dsomop_cohort_0" (the
  # second would overwrite/clash with the first). A supplied id is honoured as-is.
  if (is.null(cohort_id)) {
    cohort_id <- .cohort_auto_id(inventory)
  } else {
    candidate_id <- suppressWarnings(as.numeric(cohort_id))
    if (length(candidate_id) != 1L || is.na(candidate_id) ||
        !is.finite(candidate_id) || candidate_id < 0 ||
        candidate_id != floor(candidate_id) ||
        candidate_id > .Machine$integer.max) {
      stop("cohort_id must be one non-negative integer.", call. = FALSE)
    }
    cohort_id <- as.integer(candidate_id)
  }
  output_symbol <- paste0(".cohort_", cohort_id)
  occupied <- names(inventory)[vapply(
    inventory, function(x) output_symbol %in% x, logical(1)
  )]
  if (length(occupied) > 0L) {
    stop("Cohort output symbol '", output_symbol, "' already exists on: ",
         paste(occupied, collapse = ", "),
         ". Choose a different cohort_id.", call. = FALSE)
  }

  # Opal's DataSHIELD expression grammar cannot lex an empty string literal
  # (its string token requires >= 1 character), so a blank name would abort
  # the assign with a server-side "Lexical error" before the call even runs.
  # Fall back to a non-empty, human-readable default derived from the id.
  if (is.null(name)) {
    cohort_name <- paste0("cohort_", cohort_id)
  } else {
    if (!is.character(name) || length(name) != 1L || is.na(name) ||
        !nzchar(trimws(name))) {
      stop("name must be one non-empty string when supplied.", call. = FALSE)
    }
    cohort_name <- name
  }

  create_expr <- call(
    "omopCohortCreateDS", session$res_symbol, .ds_encode(spec), mode,
    cohort_id, cohort_name, overwrite
  )

  # Return the deterministic server-side cohort table name so it can be fed
  # straight into ds.omop.cohort.combine(). For temporary cohorts the server
  # (.cohortCreate) materialises "dsomop_cohort_<id>" and appends one `_icN`
  # suffix per inclusion criterion; persistent cohorts have no temp table.
  base_table <- paste0("dsomop_cohort_", cohort_id)
  n_criteria <- length(spec$inclusion_criteria %||% list())
  rollback_tables <- c(
    base_table,
    if (n_criteria > 0L) paste0(base_table, "_ic", seq_len(n_criteria))
  )
  table_name <- if (identical(mode, "temporary")) {
    rollback_tables[[length(rollback_tables)]]
  } else NULL

  if (identical(mode, "temporary")) {
    .cohort_assign_temporary(
      conns = conns, res_symbol = session$res_symbol,
      output_symbol = output_symbol, table_name = table_name,
      expr = create_expr, context = "Temporary cohort creation",
      session_symbol = symbol, rollback_tables = rollback_tables
    )
  } else {
    DSI::datashield.assign.expr(
      conns, symbol = output_symbol, expr = create_expr
    )
  }

  invisible(structure(
    table_name,
    symbol = output_symbol,
    class = "dsomop_cohort_handle"
  ))
}

#' Create a cohort reference for the plan DSL (client-only)
#'
#' Builds a lightweight cohort reference object that can be embedded in a
#' plan specification. This is a client-side-only helper that does not
#' contact any server; the reference is resolved at plan execution time
#' when \code{recipe_execute()} runs on the server.
#'
#' @section Disclosure control:
#' Building the reference does no checking (it never touches a server); the
#' disclosure gate applies when the reference is RESOLVED at execution time. If
#' the cohort_definition_id is unavailable -- absent OR below the server's
#' per-subset threshold (\code{nfilter_subset}) -- materialising/using it FAILS
#' CLOSED server-side (the same fail-closed gate that protects every other way of
#' naming a cohort, e.g. the \code{cohort=} scope of the exploration wrappers).
#' A sub-threshold cohort can therefore never be used to scope or populate a
#' query, exactly as if it did not exist.
#'
#' @param cohort_definition_id Integer; the ID of an existing cohort
#'   definition that has already been created on the server(s). Referencing an
#'   absent or sub-threshold id is accepted here but fails closed when resolved
#'   server-side (see Disclosure control).
#' @return A named list with class-implicit structure containing
#'   \code{type = "cohort_table"} and \code{cohort_definition_id}. Intended
#'   for use inside plan population specifications.
#' @examples
#' \dontrun{
#' ref <- ds.omop.cohort.ref(cohort_definition_id = 1)
#' # Use in a plan: plan$population <- ref
#' }
#' @export
ds.omop.cohort.ref <- function(cohort_definition_id) {
  list(
    type = "cohort_table",
    cohort_definition_id = as.integer(cohort_definition_id)
  )
}

#' Combine two cohorts with set operations
#'
#' Combines two existing server-side cohort tables using a set operation
#' (intersection, union, or set difference). The result is assigned as a
#' new server-side symbol that can be used in subsequent queries or plan
#' executions.
#'
#' @section Disclosure control:
#' Each input is resolved + re-gated server-side, and the COMBINED result is
#' gated on its distinct-subject count: if an operand is unavailable
#' (absent/sub-threshold cohort_definition_id) or the combination yields fewer
#' than the server's per-subset threshold (\code{nfilter_subset}) persons, the
#' call FAILS CLOSED and no result table is materialised. An "insufficient
#' individuals" error here reflects the operands/operation you chose and carries
#' no disclosure about any pre-existing cohort.
#'
#' @param op Character; the set operation to apply. One of
#'   \code{"intersect"} (patients in both cohorts),
#'   \code{"union"} (patients in either cohort), or
#'   \code{"setdiff"} (patients in \code{cohort_a} but not \code{cohort_b}).
#' @param cohort_a Server-side cohort TABLE name for the first cohort (the
#'   value returned by \code{ds.omop.cohort.create()}), its
#'   \code{dsomop_cohort_handle}, or a cohort definition ID (integer).
#' @param cohort_b Server-side cohort TABLE name for the second cohort (the
#'   value returned by \code{ds.omop.cohort.create()}), its
#'   \code{dsomop_cohort_handle}, or a cohort definition ID (integer).
#' @param new_name Character; TABLE name for the combined result. If
#'   \code{NULL} (the default), an auto-generated name is used.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Invisibly; a \code{dsomop_cohort_handle} carrying the server-side
#'   TABLE name for the combined cohort. The handle can itself be passed as
#'   \code{cohort_a} / \code{cohort_b} to a further \code{ds.omop.cohort.combine()}.
#' @examples
#' \dontrun{
#' diabetes <- ds.omop.cohort.create(spec = ..., cohort_id = 1)
#' hypertension <- ds.omop.cohort.create(spec = ..., cohort_id = 2)
#' # Patients with both diabetes AND hypertension
#' combined <- ds.omop.cohort.combine(
#'   op = "intersect",
#'   cohort_a = diabetes,
#'   cohort_b = hypertension
#' )
#' }
#' @export
ds.omop.cohort.combine <- function(op, cohort_a, cohort_b,
                                   new_name = NULL,
                                   symbol = "omop",
                                   conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  # Validate the operation client-side so callers fail fast instead of after a
  # server round-trip. Accept "difference" as an alias for "setdiff".
  if (!is.character(op) || length(op) != 1L || is.na(op)) {
    stop("op must be one of 'intersect', 'union', 'setdiff'.", call. = FALSE)
  }
  op <- tolower(op)
  if (identical(op, "difference")) op <- "setdiff"
  if (!op %in% c("intersect", "union", "setdiff")) {
    stop("op must be one of 'intersect', 'union', 'setdiff'.", call. = FALSE)
  }

  # Coerce handles / IDs / symbol names to the deterministic server-side TABLE
  # names the server splices directly into SQL.
  cohort_a <- .cohort_table_name(
    .resolve_cohort_table(cohort_a), "cohort_a"
  )
  cohort_b <- .cohort_table_name(
    .resolve_cohort_table(cohort_b), "cohort_b"
  )

  inventory <- .plan_symbol_inventory(conns, "cohort combination preflight")

  # Generate the result table name on the client and pass it to the server so
  # both sides agree (the server's random fallback never fires) and the
  # returned handle points at a table that actually exists.
  out_table <- if (is.null(new_name)) {
    .cohort_random_table(
      "dsomop_cohort_combined_", inventory, "cohort combination"
    )
  } else {
    .cohort_table_name(new_name, "new_name")
  }
  if (out_table %in% c(cohort_a, cohort_b)) {
    stop("new_name must differ from both input cohort tables.",
         call. = FALSE)
  }
  output_symbol <- paste0(".", out_table)

  .cohort_assign_temporary(
    conns = conns, res_symbol = session$res_symbol,
    output_symbol = output_symbol, table_name = out_table,
    expr = call("omopCohortCombineDS", session$res_symbol,
                op, cohort_a, cohort_b, out_table),
    context = "Temporary cohort combination", session_symbol = symbol
  )

  invisible(structure(
    out_table,
    symbol = output_symbol,
    class = "dsomop_cohort_handle"
  ))
}

#' Build a cohort from the persons in a server-side omop.table symbol
#'
#' Turns an existing server-side, token-keyed \code{omop.table} symbol -- e.g.
#' the symbol produced by \code{\link{ds.omop.plan.execute}} or one of the
#' data-manipulation verbs (\code{\link{ds.omop.merge}} etc.) -- into a reusable
#' cohort that can scope subsequent exploration queries and plan/recipe runs. The
#' CLIENT sends only the symbol NAME; the server reads its distinct person
#' tokens, reverses them to original ids with the per-resource key, gates the
#' distinct count (fail-closed), and materialises a size-checked cohort temp
#' table. No identifier ever leaves the server.
#'
#' @section Disclosure control:
#' The derived cohort is gated on its distinct-subject count: if the source
#' symbol resolves to fewer than the server's per-subset threshold
#' (\code{nfilter_subset}) persons, the call FAILS CLOSED with an "insufficient
#' individuals" error and no cohort table is materialised. The error reflects the
#' contents of the symbol you supplied and carries no disclosure about any
#' pre-existing cohort.
#'
#' @param x Character; the name of a server-side \code{omop.table} symbol.
#' @param new_name Character; TABLE name for the cohort. If \code{NULL} (the
#'   default), an auto-generated name is used.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Invisibly; a \code{dsomop_cohort_handle} carrying the server-side
#'   TABLE name. Pass it straight into the \code{cohort} argument of the
#'   exploration wrappers (e.g. \code{ds.omop.concept.prevalence}), into
#'   \code{ds.omop.cohort.combine()}, or as a plan/recipe population scope.
#' @examples
#' \dontrun{
#' feats <- ds.omop.plan.execute(plan, out = c(features = "F"))
#' coh <- ds.omop.cohort.from_table("F")
#' ds.omop.concept.prevalence("condition_occurrence", cohort = coh)
#' }
#' @seealso \code{\link{ds.omop.cohort.create}}, \code{\link{ds.omop.cohort.combine}}
#' @export
ds.omop.cohort.from_table <- function(x, new_name = NULL,
                                      symbol = "omop", conns = NULL) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[A-Za-z.][A-Za-z0-9._]*$", x) || grepl("^\\.[0-9]", x)) {
    stop("x must be the name of a server-side omop.table symbol.",
         call. = FALSE)
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  inventory <- .plan_symbol_inventory(conns, "cohort-from-table preflight")

  # Generate the result table name on the client and pass it to the server so
  # both sides agree (the server's random fallback never fires) and the returned
  # handle points at a table that exists and can be named in later cohort= calls.
  out_table <- if (is.null(new_name)) {
    .cohort_random_table(
      "dsomop_cohort_fromtbl_", inventory, "cohort-from-table creation"
    )
  } else {
    .cohort_table_name(
      new_name, "new_name",
      "^dsomop_cohort_fromtbl_[A-Za-z0-9_]{4,64}$"
    )
  }
  output_symbol <- paste0(".", out_table)

  .cohort_assign_temporary(
    conns = conns, res_symbol = session$res_symbol,
    output_symbol = output_symbol, table_name = out_table,
    expr = call("omopCohortFromTableDS", as.name(x),
                session$res_symbol, out_table),
    context = "Temporary cohort-from-table creation",
    session_symbol = symbol, required_symbols = x
  )

  invisible(structure(
    out_table,
    symbol = output_symbol,
    class = "dsomop_cohort_handle"
  ))
}

#' Resolve a cohort reference to its server-side table name
#'
#' Maps the various forms a caller may supply for a cohort -- a
#' \code{dsomop_cohort_handle} (as returned by \code{ds.omop.cohort.create}
#' or \code{ds.omop.cohort.combine}), a cohort definition ID, or a server-side
#' name string -- to the deterministic temp table name the server expects.
#'
#' @param x A \code{dsomop_cohort_handle}, a numeric cohort definition ID, or
#'   a character table/symbol name.
#' @return Character; the server-side cohort table name.
#' @keywords internal
.resolve_cohort_table <- function(x) {
  if (inherits(x, "dsomop_cohort_handle")) {
    return(unclass(x)[1])
  }
  if (is.numeric(x)) {
    return(paste0("dsomop_cohort_", as.integer(x)))
  }
  if (is.character(x) && grepl("^\\.cohort_", x)) {
    return(sub("^\\.cohort_", "dsomop_cohort_", x))
  }
  x
}

#' Resolve the unified \code{cohort=} scope argument of the exploration wrappers
#'
#' The exploration wrappers accept a single \code{cohort} argument naming the
#' population to scope to. This maps the accepted forms to the value the SERVER's
#' \code{.resolveCohortArg}/\code{.resolveCohortTable} expects, which then
#' materialises + re-gates it server-side:
#' \itemize{
#'   \item a \code{dsomop_cohort_handle} (from \code{ds.omop.cohort.create},
#'     \code{.combine}, or \code{.from_table}) -> its server TABLE name;
#'   \item a numeric cohort_definition_id -> the integer, passed through so the
#'     server materialises it from the cohort results table;
#'   \item a character TABLE name -> as-is;
#'   \item \code{NULL} -> \code{NULL} (no scoping).
#' }
#' This deliberately does NOT collapse a numeric id to a \code{dsomop_cohort_<id>}
#' temp-table name (that is \code{.resolve_cohort_table}'s job for the set-ops
#' path); for exploration a bare id means a cohort_definition_id.
#'
#' @param cohort The unified \code{cohort} argument.
#' @return A server-side cohort table name, a cohort_definition_id, or NULL.
#' @keywords internal
.cohort_scope_arg <- function(cohort) {
  if (is.null(cohort)) return(NULL)
  if (inherits(cohort, "dsomop_cohort_handle")) return(unclass(cohort)[1])
  if (is.numeric(cohort)) return(as.integer(cohort))
  cohort
}
