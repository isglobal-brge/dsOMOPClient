# Module: OHDSI Results Consumer (Client)
# Client-side wrappers for querying pre-computed OHDSI tool result tables.

#' Check OHDSI result tool availability
#'
#' Queries each connected server to determine which OHDSI tool result tables
#' (DQD, CohortDiagnostics, CohortIncidence, Characterization) are present.
#'
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses active session.
#' @return A \code{dsomop_result} object with \code{scope = "per_site"}.
#' @examples
#' \dontrun{
#' status <- ds.omop.ohdsi.status()
#' status$per_site[["server_a"]]$dqd$available
#' }
#' @export
ds.omop.ohdsi.status <- function(symbol = "omop", conns = NULL) {
  code <- .build_code("ds.omop.ohdsi.status", symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopOhdsiStatusDS", session$res_symbol)
  )

  dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = "per_site"))
}

#' List discovered OHDSI result tables
#'
#' Returns a catalog of all OHDSI result tables found across connected servers,
#' including tool identification and row counts.
#'
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses active session.
#' @return A \code{dsomop_result} object. Pooled is the union of catalogs.
#' @examples
#' \dontrun{
#' tables <- ds.omop.ohdsi.tables()
#' tables$pooled
#' }
#' @export
ds.omop.ohdsi.tables <- function(symbol = "omop", conns = NULL) {
  code <- .build_code("ds.omop.ohdsi.tables", symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopOhdsiTablesDS", session$res_symbol)
  )

  # Pool: union of table catalogs
  all_dfs <- list()
  for (srv in names(raw)) {
    df <- raw[[srv]]
    if (is.data.frame(df) && nrow(df) > 0) {
      df$.server <- srv
      all_dfs[[srv]] <- df
    }
  }
  pooled <- if (length(all_dfs) > 0) {
    result <- do.call(rbind, all_dfs)
    rownames(result) <- NULL
    result
  } else NULL

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = "pooled"))
}

#' Query an OHDSI result table
#'
#' Reads rows from a pre-computed OHDSI result table with server-controlled
#' disclosure. When \code{scope = "pooled"}, counts are summed across servers
#' with suppression propagation; rates/proportions are set to NA.
#'
#' @param table_name Character; which result table to query.
#' @param columns Character vector; columns to select (NULL = all safe columns).
#' @param filters Named list; WHERE conditions.
#' @param order_by Character; ORDER BY column.
#' @param limit Integer; max rows (capped at 5000 server-side).
#' @param tool_id Character; optional tool identifier.
#' @param scope Character; \code{"per_site"} or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses active session.
#' @return A \code{dsomop_result} object.
#' @examples
#' \dontrun{
#' # DQD results per site
#' dqd <- ds.omop.ohdsi.results("dqdashboard_results")
#'
#' # Cohort counts pooled across servers
#' cc <- ds.omop.ohdsi.results("cohort_count", scope = "pooled")
#' }
#' @export
ds.omop.ohdsi.results <- function(table_name, columns = NULL,
                                    filters = NULL, order_by = NULL,
                                    limit = 5000L, tool_id = NULL,
                                    scope = c("per_site", "pooled"),
                                    pooling_policy = "strict",
                                    symbol = "omop", conns = NULL) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.ohdsi.results",
    table_name = table_name, scope = scope, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopOhdsiResultsDS", session$res_symbol,
                table_name, columns, filters, order_by,
                as.integer(limit), tool_id)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "ohdsi_results", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get OHDSI tool summary
#'
#' Returns a summary of available result tables for a specific OHDSI tool.
#'
#' @param tool_id Character; which tool to summarize (e.g., \code{"dqd"},
#'   \code{"cohort_diagnostics"}, \code{"cohort_incidence"},
#'   \code{"characterization"}).
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses active session.
#' @return A \code{dsomop_result} object with \code{scope = "per_site"}.
#' @examples
#' \dontrun{
#' dqd_summary <- ds.omop.ohdsi.summary("dqd")
#' dqd_summary$per_site
#' }
#' @export
ds.omop.ohdsi.summary <- function(tool_id, symbol = "omop", conns = NULL) {
  code <- .build_code("ds.omop.ohdsi.summary",
    tool_id = tool_id, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopOhdsiSummaryDS", session$res_symbol, tool_id)
  )

  dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = "per_site"))
}
