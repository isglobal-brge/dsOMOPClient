# ==============================================================================
# dsOMOPClient v2 - Achilles Client Wrappers (Data Sources)
# ==============================================================================
# Four exported functions for querying Achilles pre-computed aggregate
# statistics. Each follows the existing wrapper pattern from exploration.R.
# ==============================================================================

# --- Client Wrappers ---------------------------------------------------------

#' Check Achilles availability
#'
#' Returns a dsomop_result with per-site availability status and table counts.
#' No pooling needed (metadata only).
#'
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections (NULL = use session)
#' @return A dsomop_result object
#' @export
ds.omop.achilles.status <- function(symbol = "omop", conns = NULL) {
  code <- .build_code("ds.omop.achilles.status", symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopAchillesStatusDS", session$res_symbol)
  )

  dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = "per_site"))
}

#' List available Achilles analyses
#'
#' Returns the analysis catalog, optionally filtered by domain.
#' Pooling picks first server (catalog is identical across servers).
#'
#' @param domain Character; optional domain filter ("person", "condition",
#'   "drug", "measurement", "procedure", "observation", "visit", "device")
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections (NULL = use session)
#' @return A dsomop_result object
#' @export
ds.omop.achilles.analyses <- function(domain = NULL, symbol = "omop",
                                       conns = NULL) {
  code <- .build_code("ds.omop.achilles.analyses",
    domain = domain, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopAchillesAnalysesDS", session$res_symbol, domain)
  )

  # Pick first server for pooled (catalog is identical)
  pooled <- if (length(raw) > 0) raw[[1]] else NULL

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = "pooled"))
}

#' Get Achilles count results
#'
#' Queries achilles_results for specified analysis IDs with optional stratum
#' filtering. Supports pooling via count summation across servers.
#'
#' @param analysis_ids Integer vector; which analysis IDs to retrieve
#' @param stratum_filters Named list; e.g. list(stratum_1 = "201820")
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections (NULL = use session)
#' @param execute Logical; if FALSE, return dry-run with code only
#' @return A dsomop_result object
#' @export
ds.omop.achilles.results <- function(analysis_ids,
                                      stratum_filters = NULL,
                                      scope = c("per_site", "pooled"),
                                      pooling_policy = "strict",
                                      symbol = "omop", conns = NULL,
                                      execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.achilles.results",
    analysis_ids = analysis_ids, stratum_filters = stratum_filters,
    scope = scope, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopAchillesResultsDS", session$res_symbol,
                as.integer(analysis_ids), stratum_filters, NULL)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "achilles_results", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get Achilles distribution results
#'
#' Queries achilles_results_dist for distribution analyses with optional
#' stratum filtering. Supports pooling via weighted aggregation.
#'
#' @param analysis_ids Integer vector; distribution analysis IDs
#' @param stratum_filters Named list; optional stratum filters
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections (NULL = use session)
#' @param execute Logical; if FALSE, return dry-run with code only
#' @return A dsomop_result object
#' @export
ds.omop.achilles.distribution <- function(analysis_ids,
                                            stratum_filters = NULL,
                                            scope = c("per_site", "pooled"),
                                            pooling_policy = "strict",
                                            symbol = "omop", conns = NULL,
                                            execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.achilles.distribution",
    analysis_ids = analysis_ids, stratum_filters = stratum_filters,
    scope = scope, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopAchillesDistributionDS", session$res_symbol,
                as.integer(analysis_ids), stratum_filters)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "achilles_distribution", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get Achilles analysis catalog
#'
#' Returns the full catalog of available Achilles analyses from the server.
#' If the achilles_analysis table exists, returns its contents; otherwise
#' discovers available analyses from achilles_results.
#'
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections (NULL = use session)
#' @return A dsomop_result object
#' @export
ds.omop.achilles.catalog <- function(symbol = "omop", conns = NULL) {
  code <- .build_code("ds.omop.achilles.catalog", symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopAchillesCatalogDS", session$res_symbol)
  )

  # Pick first server for pooled (catalog is identical)
  pooled <- if (length(raw) > 0) raw[[1]] else NULL

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = "pooled"))
}
