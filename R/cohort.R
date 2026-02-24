# ==============================================================================
# dsOMOPClient v2 - Cohort Management
# ==============================================================================

#' List available cohort definitions
#'
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
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
#' @param id Integer; cohort definition ID
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) with definition details
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
#' @param spec Named list defining the cohort
#' @param mode Character; "temporary" or "persistent"
#' @param cohort_id Integer; cohort definition ID
#' @param name Character; human-readable name
#' @param overwrite Logical; overwrite existing
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Character (per server); temp table name or confirmation
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
#' @param cohort_definition_id Integer; ID of existing cohort
#' @return List; a cohort reference for plans
#' @export
ds.omop.cohort.ref <- function(cohort_definition_id) {
  list(
    type = "cohort_table",
    cohort_definition_id = as.integer(cohort_definition_id)
  )
}

#' Combine two cohorts with set operations
#'
#' @param op Character; "intersect", "union", or "setdiff"
#' @param cohort_a Character; first cohort temp table
#' @param cohort_b Character; second cohort temp table
#' @param new_name Character; name for result
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Invisible; combined cohort reference
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
