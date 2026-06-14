# Module: Cohort Operations
# Client-side wrappers for cohort creation, combination, listing, and retrieval.

#' List available cohort definitions
#'
#' Queries each connected server for existing cohort definitions. Returns a
#' named list (one entry per server) of data frames containing cohort IDs,
#' names, and row counts. This is useful for discovering cohorts that have
#' already been created in persistent storage.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Named list (one entry per server) of data frames with cohort
#'   metadata. Returns an empty list if no cohorts are defined.
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
#' @param id Integer; the cohort definition ID to retrieve. Must correspond
#'   to an existing cohort on the server(s).
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Named list (one entry per server) with definition details,
#'   including the cohort specification and member count.
#' @examples
#' \dontrun{
#' defn <- ds.omop.cohort.definition(id = 1)
#' defn[["server_a"]]
#' }
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
#' @param spec Named list defining the cohort. Must contain at least
#'   \code{type} (character) and \code{concept_set} (integer vector or
#'   \code{omop_concept_set} object).
#' @param mode Character; \code{"temporary"} (the default) creates a
#'   session-scoped temp table, \code{"persistent"} writes to the cohort
#'   schema for reuse across sessions.
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
#'   \code{"dsomop_cohort_1"}) for a temporary cohort, or \code{NULL} for a
#'   persistent cohort. The table is assigned server-side via
#'   \code{DSI::datashield.assign.expr}. Pass the returned handle straight
#'   into \code{ds.omop.cohort.combine()}.
#' @examples
#' \dontrun{
#' diabetes <- ds.omop.cohort.create(
#'   spec = list(type = "event",
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
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  if (is.null(spec$type)) {
    stop("Cohort spec must include 'type'.", call. = FALSE)
  }
  if (is.null(spec$concept_set) || length(spec$concept_set) == 0) {
    stop("Cohort spec must include 'concept_set'.",
         call. = FALSE)
  }

  # Opal's DataSHIELD expression grammar cannot lex an empty string literal
  # (its string token requires >= 1 character), so a blank name would abort
  # the assign with a server-side "Lexical error" before the call even runs.
  # Fall back to a non-empty, human-readable default derived from the id.
  cohort_name <- if (is.null(name) || !nzchar(trimws(name)))
    paste0("cohort_", cohort_id %||% "tmp") else name

  DSI::datashield.assign.expr(
    conns,
    symbol = paste0(".cohort_", cohort_id %||% "tmp"),
    expr = call("omopCohortCreateDS",
                session$res_symbol,
                .ds_encode(spec), mode,
                as.integer(cohort_id %||% 0L),
                cohort_name,
                overwrite)
  )

  # Return the deterministic server-side cohort table name so it can be fed
  # straight into ds.omop.cohort.combine(). For temporary cohorts the server
  # (.cohortCreate) materialises a table named "dsomop_cohort_<id>"; persistent
  # cohorts write to the cohort schema and have no temp table.
  table_name <- if (identical(mode, "temporary"))
    paste0("dsomop_cohort_", as.integer(cohort_id %||% 0L)) else NULL

  invisible(structure(
    table_name,
    symbol = paste0(".cohort_", cohort_id %||% "tmp"),
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
#' @param cohort_definition_id Integer; the ID of an existing cohort
#'   definition that has already been created on the server(s).
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
  op <- tolower(op)
  if (identical(op, "difference")) op <- "setdiff"
  if (!op %in% c("intersect", "union", "setdiff")) {
    stop("op must be one of 'intersect', 'union', 'setdiff'.", call. = FALSE)
  }

  # Coerce handles / IDs / symbol names to the deterministic server-side TABLE
  # names the server splices directly into SQL.
  cohort_a <- .resolve_cohort_table(cohort_a)
  cohort_b <- .resolve_cohort_table(cohort_b)

  # Generate the result table name on the client and pass it to the server so
  # both sides agree (the server's random fallback never fires) and the
  # returned handle points at a table that actually exists.
  out_table <- new_name %||% paste0(
    "dsomop_cohort_combined_", sample(1000:9999, 1))

  DSI::datashield.assign.expr(
    conns,
    symbol = paste0(".", out_table),
    expr = call("omopCohortCombineDS",
                session$res_symbol,
                op, cohort_a, cohort_b, out_table)
  )

  invisible(structure(
    out_table,
    symbol = paste0(".", out_table),
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
  if (!is.character(x) || length(x) != 1L) {
    stop("x must be the name of a server-side omop.table symbol.",
         call. = FALSE)
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  # Generate the result table name on the client and pass it to the server so
  # both sides agree (the server's random fallback never fires) and the returned
  # handle points at a table that exists and can be named in later cohort= calls.
  out_table <- new_name %||% paste0(
    "dsomop_cohort_fromtbl_", sample(1000:9999, 1))

  DSI::datashield.assign.expr(
    conns,
    symbol = paste0(".", out_table),
    expr = call("omopCohortFromTableDS", as.name(x),
                session$res_symbol, out_table)
  )

  invisible(structure(
    out_table,
    symbol = paste0(".", out_table),
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
