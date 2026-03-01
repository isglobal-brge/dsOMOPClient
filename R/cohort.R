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

  DSI::datashield.aggregate(
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

  DSI::datashield.aggregate(
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
#' @return Invisibly; the server-side symbol name for the created cohort
#'   table (character). Assigned via \code{DSI::datashield.assign.expr}.
#' @examples
#' \dontrun{
#' ds.omop.cohort.create(
#'   spec = list(type = "event",
#'               concept_set = c(201820, 201826)),
#'   cohort_id = 1,
#'   name = "Type 2 Diabetes"
#' )
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

  DSI::datashield.assign.expr(
    conns,
    symbol = paste0(".cohort_", cohort_id %||% "tmp"),
    expr = call("omopCohortCreateDS",
                session$res_symbol,
                spec, mode,
                as.integer(cohort_id %||% 0L),
                name %||% "",
                overwrite)
  )
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
#' @param cohort_a Character; server-side symbol name for the first cohort
#'   temp table.
#' @param cohort_b Character; server-side symbol name for the second cohort
#'   temp table.
#' @param new_name Character; symbol name for the combined result. If
#'   \code{NULL} (the default), an auto-generated name is used.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Invisibly; the server-side symbol name (character) for the
#'   combined cohort table.
#' @examples
#' \dontrun{
#' # Patients with both diabetes AND hypertension
#' combined <- ds.omop.cohort.combine(
#'   op = "intersect",
#'   cohort_a = ".cohort_1",
#'   cohort_b = ".cohort_2"
#' )
#' }
#' @export
ds.omop.cohort.combine <- function(op, cohort_a, cohort_b,
                                   new_name = NULL,
                                   symbol = "omop",
                                   conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  out_name <- new_name %||% paste0(
    ".cohort_combined_", sample(1000:9999, 1))

  DSI::datashield.assign.expr(
    conns,
    symbol = out_name,
    expr = call("omopCohortCombineDS",
                session$res_symbol,
                op, cohort_a, cohort_b, new_name)
  )

  invisible(out_name)
}
