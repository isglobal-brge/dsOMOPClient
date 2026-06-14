# Module: Unified Analysis Catalog (Client)
# Client-side wrappers for the server's single analysis catalog, which folds the
# curated QueryLibrary SQL templates, the pre-computed Achilles analyses, and the
# generic OHDSI result tables behind ONE registry, ONE fail-closed run path, and
# ONE per-patient disclosure gate (see dsOMOP/R/analysis_catalog.R). Entries are
# named with a stable, pack-prefixed id (\code{"dsomop:<id>"}).
#
# These wrappers mirror the achilles/ohdsi wrapper style: list/get return the
# (server-identical) catalog metadata; run executes one entry across every
# connected server and returns a \code{dsomop_result}. Population scoping is
# expressed by a cohort reference and/or one or more workspace \code{omop.table}
# symbols, folded with \code{combine} server-side into one re-gated cohort.

#' Build the server-side scope expression for an analysis run
#'
#' The server's \code{omopAnalysisRunDS}/\code{omopAnalysisRunAssignDS} take a
#' single \code{scope} argument that may mix a cohort reference (a temp-table
#' name or a \code{cohort_definition_id}) with one or more workspace
#' \code{omop.table} frames. On the client we hold the \code{omop.table}s only by
#' SYMBOL NAME, so we splice them into the call as unevaluated symbols
#' (\code{as.name}) — exactly as the data-manipulation verbs and
#' \code{omopFactorLevelsDS} do — and let DataSHIELD resolve each name to the
#' server-side frame. The cohort reference travels as a literal value.
#'
#' Forms produced:
#' \itemize{
#'   \item no cohort, no tables -> \code{NULL} (no scoping argument).
#'   \item a cohort only -> the resolved cohort value (string / id), passed as-is.
#'   \item table symbol(s) (with or without a cohort) -> a \code{list(...)} call
#'     whose elements are the cohort value (if any) followed by one
#'     \code{as.name(<symbol>)} per table.
#' }
#'
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side table name) or \code{NULL}.
#' @param tables Character vector of server-side \code{omop.table} symbol names,
#'   or \code{NULL}.
#' @return \code{NULL}, a single literal cohort value, or a \code{call} to
#'   \code{list()} mixing the cohort literal and table symbols.
#' @keywords internal
.analysis_scope_expr <- function(cohort = NULL, tables = NULL) {
  cohort_val <- .cohort_scope_arg(cohort)

  if (!is.null(tables)) {
    if (!is.character(tables)) {
      stop("tables must be the name(s) of server-side omop.table symbol(s).",
           call. = FALSE)
    }
    tables <- tables[nzchar(tables)]
  }

  if (is.null(tables) || length(tables) == 0) {
    # Cohort-only (or nothing): pass the cohort value straight through, matching
    # the exploration wrappers' single-cohort scope.
    return(cohort_val)
  }

  # One or more table symbols (optionally plus a cohort): build a list() call so
  # the server receives a mixed scope of a cohort literal + omop.table frames.
  elems <- c(
    if (!is.null(cohort_val)) list(cohort_val) else list(),
    lapply(tables, as.name)
  )
  as.call(c(as.name("list"), elems))
}

#' Build the (possibly scope-bearing) server-side analysis run call
#'
#' Constructs the unevaluated DataSHIELD call for \code{omopAnalysisRunDS} /
#' \code{omopAnalysisRunAssignDS}. \code{params} is JSON/base64-encoded for Opal
#' transport (\code{\link{.ds_encode}}); \code{scope_expr} (when present) is
#' spliced in by NAME so a \code{list(as.name(<table>))} scope resolves to the
#' server-side \code{omop.table} frames, and \code{combine} is passed by name so
#' a \code{NULL} scope never shifts it into the wrong positional slot.
#'
#' @param fn Character; the server method name.
#' @param res_symbol Character; the server-side handle symbol.
#' @param name Character; the catalog entry name.
#' @param params Named list of parameter values.
#' @param scope_expr \code{NULL}, a literal cohort value, or a \code{list(...)}
#'   call (from \code{\link{.analysis_scope_expr}}).
#' @param combine Character; \code{"union"} or \code{"intersect"}.
#' @return An unevaluated \code{call}.
#' @keywords internal
.analysis_run_call <- function(fn, res_symbol, name, params, scope_expr,
                               combine) {
  args <- list(as.name(fn), res_symbol, name, .ds_encode(params))
  if (!is.null(scope_expr)) {
    args <- c(args, list(scope = scope_expr))
  }
  args <- c(args, list(combine = combine))
  as.call(args)
}

#' List unified analysis catalog entries
#'
#' Returns metadata for every entry in the server's unified analysis catalog —
#' the single registry that folds the curated QueryLibrary SQL templates, the
#' pre-computed Achilles analyses, and the generic OHDSI result tables behind one
#' stable, pack-prefixed naming scheme (\code{"dsomop:<id>"}). Because the
#' catalog is defined by the server package, it is identical across servers and
#' the first responding server's result is returned as the pooled view. No SQL,
#' compute functions, or other server internals are exposed.
#'
#' @param domain Character; optional clinical-domain filter (e.g.
#'   \code{"condition"}, \code{"person"}). \code{NULL} (the default) returns
#'   entries for all domains.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"}. The pooled
#'   element is a data frame with one row per entry: \code{name}, \code{domain},
#'   \code{adapter}, \code{mode}, disclosure \code{unit}, \code{description},
#'   parameter summary, and the \code{accepts_cohort}/\code{accepts_tables}
#'   scoping flags.
#' @examples
#' \dontrun{
#' catalog <- ds.omop.analysis.list()
#' head(catalog$pooled)
#'
#' # Only condition-domain analyses
#' cond <- ds.omop.analysis.list(domain = "condition")
#' cond$pooled$name
#' }
#' @seealso \code{\link{ds.omop.analysis.get}}, \code{\link{ds.omop.analysis.run}}
#' @export
ds.omop.analysis.list <- function(domain = NULL, symbol = "omop",
                                  conns = NULL) {
  code <- .build_code("ds.omop.analysis.list", domain = domain, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAnalysisListDS", session$res_symbol, domain)
  )

  # Catalog is identical across servers: pooled view is the first server's.
  pooled <- if (length(raw) > 0) raw[[1]] else NULL

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = "pooled"))
}

#' Get unified analysis catalog entry metadata
#'
#' Returns full metadata for a single catalog entry: its parameter specs,
#' compute kind, disclosure spec, and scoping capabilities. Use it to discover
#' an entry's parameters and to check whether it accepts cohort/table scoping
#' before running it. The metadata is identical across servers, so the first
#' responding server's result is returned as the pooled view.
#'
#' @param name Character; the entry name (pack-prefixed stable id, e.g.
#'   \code{"dsomop:achilles.401"}).
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"}. The pooled
#'   element is a named list with the entry's \code{name}, \code{description},
#'   \code{domain}, \code{mode}, \code{params}, \code{compute_kind},
#'   \code{disclosure}, \code{scope}, and \code{adapter}.
#' @examples
#' \dontrun{
#' meta <- ds.omop.analysis.get("dsomop:achilles.401")
#' meta$pooled$params
#' meta$pooled$mode
#' }
#' @seealso \code{\link{ds.omop.analysis.list}}, \code{\link{ds.omop.analysis.run}}
#' @export
ds.omop.analysis.get <- function(name, symbol = "omop", conns = NULL) {
  code <- .build_code("ds.omop.analysis.get", name = name, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAnalysisGetDS", session$res_symbol, name)
  )

  # Metadata is identical across servers: pooled view is the first server's.
  pooled <- if (length(raw) > 0) raw[[1]] else NULL

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = "pooled"))
}

#' Run a unified analysis catalog entry
#'
#' Executes one catalog entry across every connected server through the server's
#' single fail-closed run path, which validates and sanitizes parameters, applies
#' optional population scoping, runs the entry's compute step (SQL template or
#' wrapped Achilles/OHDSI accessor), and funnels the result through the ONE
#' per-patient disclosure gate. Aggregate entries return disclosure-controlled
#' data frames that are additionally pooled across servers (count columns are
#' summed with suppression propagation). Assign-mode QueryLibrary loaders
#' (detected from the entry metadata) instead store their result server-side and
#' return per-server assignment confirmations.
#'
#' Scoping: pass a \code{cohort} reference and/or one or more workspace
#' \code{omop.table} symbol names in \code{tables}. Multiple sources are folded
#' server-side with \code{combine} (\code{"union"}/\code{"intersect"} on the
#' person key) into a single re-gated cohort, and SQL entries are restricted to
#' it. Pre-computed Achilles/OHDSI entries hold no per-row person key and reject
#' scoping (the server raises a clear error).
#'
#' @param name Character; the entry name (pack-prefixed stable id).
#' @param params Named list; parameter values for the entry (see
#'   \code{\link{ds.omop.analysis.get}} for the entry's parameter specs).
#' @param cohort Optional cohort reference to scope the population to: a
#'   \code{dsomop_cohort_handle} (from \code{\link{ds.omop.cohort.create}},
#'   \code{\link{ds.omop.cohort.combine}}, or \code{\link{ds.omop.cohort.from_table}}),
#'   a \code{cohort_definition_id}, or a server-side cohort table name.
#'   \code{NULL} (the default) means no cohort scoping.
#' @param tables Optional character vector of server-side \code{omop.table}
#'   symbol names to scope the population to (their distinct persons). May be
#'   combined with \code{cohort}.
#' @param combine Character; how to fold multiple scope sources together:
#'   \code{"union"} (the default) or \code{"intersect"}.
#' @param pooling_policy Character; how suppressed (NA) cells are handled when
#'   pooling aggregate results across servers. \code{"strict"} (the default)
#'   sets the pooled value to NA if any server suppressed it; \code{"pooled_only_ok"}
#'   sums only the non-suppressed values.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return A \code{dsomop_result} object. For aggregate entries, \code{per_site}
#'   holds each server's disclosure-controlled data frame and \code{pooled} holds
#'   the cross-server aggregation. For assign-mode entries, \code{per_site} holds
#'   per-server assignment confirmations (the data stays on the server) and the
#'   server-side symbol name is recorded in the result metadata.
#' @examples
#' \dontrun{
#' # Discover, inspect, then run an entry scoped to a cohort.
#' catalog <- ds.omop.analysis.list(domain = "condition")
#' entry   <- catalog$pooled$name[1]
#' meta    <- ds.omop.analysis.get(entry)
#' meta$pooled$params
#'
#' diabetes <- ds.omop.cohort.create(spec = ..., cohort_id = 1)
#' res <- ds.omop.analysis.run(
#'   entry,
#'   params = list(top_n = 25),
#'   cohort = diabetes
#' )
#' res$pooled
#'
#' # Scope by one or more workspace omop.table symbols, intersected.
#' res2 <- ds.omop.analysis.run(
#'   entry,
#'   tables  = c("my_cohort", "my_other_cohort"),
#'   combine = "intersect"
#' )
#' }
#' @seealso \code{\link{ds.omop.analysis.list}}, \code{\link{ds.omop.analysis.get}}
#' @export
ds.omop.analysis.run <- function(name, params = list(), cohort = NULL,
                                 tables = NULL, combine = "union",
                                 pooling_policy = "strict",
                                 symbol = "omop", conns = NULL) {
  combine <- match.arg(combine, c("union", "intersect"))

  code <- .build_code("ds.omop.analysis.run", name = name, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  # Decide aggregate vs assign from the entry's mode (assign loaders keep their
  # result server-side and go through the assign run path).
  is_assign <- tryCatch({
    meta <- DSI::datashield.aggregate(
      conns, expr = call("omopAnalysisGetDS", session$res_symbol, name))
    m <- if (length(meta) > 0) meta[[1]] else NULL
    identical(m$mode, "assign")
  }, error = function(e) FALSE)

  scope_expr <- .analysis_scope_expr(cohort, tables)

  if (is_assign) {
    # Assign-mode loader: the server stores the result; nothing returns to pool.
    newobj <- .generate_symbol("omop.analysis")
    run_expr <- .analysis_run_call(
      "omopAnalysisRunAssignDS", session$res_symbol, name, params,
      scope_expr, combine)
    per_site <- list()
    for (srv in names(conns)) {
      ok <- tryCatch({
        DSI::datashield.assign.expr(conns[srv], symbol = newobj,
                                    expr = run_expr)
        TRUE
      }, error = function(e) e$message)
      per_site[[srv]] <- ok
    }
    return(dsomop_result(
      per_site = per_site, pooled = NULL,
      meta = list(call_code = code, scope = "per_site",
                  assign_symbol = newobj)))
  }

  # Aggregate entry: run on each server, then pool the returned frames.
  run_expr <- .analysis_run_call(
    "omopAnalysisRunDS", session$res_symbol, name, params,
    scope_expr, combine)
  raw <- .ds_safe_aggregate(conns, expr = run_expr)

  pool_out <- .pool_result(raw, "ohdsi_results", pooling_policy)

  dsomop_result(
    per_site = raw, pooled = pool_out$result,
    meta = list(call_code = code, scope = "pooled",
                pooling_policy = pooling_policy,
                warnings = pool_out$warnings))
}
