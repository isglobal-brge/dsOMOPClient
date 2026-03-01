# Module: Achilles Analytics
# Client-side wrappers for querying pre-computed Achilles aggregate statistics.

#' Check Achilles availability
#'
#' Queries each connected server to determine whether Achilles result tables
#' (\code{achilles_results} and \code{achilles_results_dist}) are present
#' and accessible. Returns per-site availability status and table row counts.
#' No pooling is performed because this is metadata-only.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return A \code{dsomop_result} object with \code{scope = "per_site"}.
#'   Each server's result is a list or data frame indicating whether the
#'   Achilles tables exist and how many rows they contain.
#' @examples
#' \dontrun{
#' status <- ds.omop.achilles.status()
#' status$per_site[["server_a"]]$available
#' }
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
#' Returns the catalog of Achilles analyses available on the connected servers,
#' optionally filtered by clinical domain. Since the catalog is identical
#' across servers (it is defined by the Achilles specification), the pooled
#' result is taken from the first responding server.
#'
#' @param domain Character; optional domain filter. Valid values include
#'   \code{"person"}, \code{"condition"}, \code{"drug"}, \code{"measurement"},
#'   \code{"procedure"}, \code{"observation"}, \code{"visit"}, and
#'   \code{"device"}. \code{NULL} (the default) returns all domains.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"}.
#'   The pooled result is a data frame listing analysis IDs, names, and
#'   associated domains.
#' @examples
#' \dontrun{
#' # All available analyses
#' catalog <- ds.omop.achilles.analyses()
#' catalog$pooled
#'
#' # Only condition-related analyses
#' cond <- ds.omop.achilles.analyses(domain = "condition")
#' }
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
#' Queries the \code{achilles_results} table on each connected server for
#' the specified analysis IDs. These are pre-computed count-based statistics
#' (e.g., person counts by gender, condition frequency). When
#' \code{scope = "pooled"}, counts are summed across servers with suppression
#' propagation: if any server suppressed a cell (NA), the pooled cell is also
#' set to NA to prevent disclosure.
#'
#' @param analysis_ids Integer vector; the Achilles analysis IDs to retrieve
#'   (e.g., \code{c(1, 2, 400)} for total persons, gender breakdown, and
#'   condition frequency).
#' @param scope Character; \code{"per_site"} returns each server's results
#'   separately, \code{"pooled"} additionally aggregates counts across servers.
#' @param pooling_policy Character; controls how suppressed (NA) cells are
#'   handled during pooling. \code{"strict"} (the default) sets the pooled
#'   value to NA if any server suppressed the cell. \code{"pooled_only_ok"}
#'   sums only the non-suppressed values.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @return A \code{dsomop_result} object. The \code{per_site} element contains
#'   per-server data frames with columns \code{analysis_id}, \code{stratum_1}
#'   through \code{stratum_5}, and \code{count_value}. The \code{pooled}
#'   element (if requested) contains the aggregated data frame.
#' @examples
#' \dontrun{
#' # Get total person count (analysis 1) pooled across servers
#' res <- ds.omop.achilles.results(analysis_ids = 1, scope = "pooled")
#' res$pooled
#'
#' # Get gender breakdown per site
#' gender <- ds.omop.achilles.results(analysis_ids = 2)
#' gender$per_site
#' }
#' @export
ds.omop.achilles.results <- function(analysis_ids,
                                      scope = c("per_site", "pooled"),
                                      pooling_policy = "strict",
                                      symbol = "omop", conns = NULL,
                                      execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.achilles.results",
    analysis_ids = analysis_ids,
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
                as.integer(analysis_ids))
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
#' Queries the \code{achilles_results_dist} table on each connected server
#' for the specified distribution analysis IDs (e.g., age distribution,
#' observation period length). Distribution analyses store percentile values
#' (p10, p25, median, p75, p90) rather than raw counts. Extreme values
#' (min/max) are never returned by the server for disclosure protection.
#'
#' When \code{scope = "pooled"}, pooling is attempted via weighted
#' aggregation; however, percentile values cannot be meaningfully combined
#' from pre-computed summaries, so all percentile columns in the pooled
#' result are set to \code{NA}. Use per-site data for percentile display.
#'
#' @param analysis_ids Integer vector; distribution analysis IDs to retrieve
#'   (e.g., \code{c(103, 105)} for age and observation length distributions).
#' @param scope Character; \code{"per_site"} returns each server's results
#'   separately, \code{"pooled"} additionally attempts weighted aggregation.
#' @param pooling_policy Character; controls how suppressed (NA) cells are
#'   handled during pooling. \code{"strict"} (the default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @return A \code{dsomop_result} object. Per-site results contain
#'   distribution data frames with columns \code{analysis_id},
#'   \code{stratum_1}, \code{count_value}, \code{avg_value},
#'   \code{stdev_value}, \code{p10_value}, \code{p25_value},
#'   \code{median_value}, \code{p75_value}, \code{p90_value}.
#' @examples
#' \dontrun{
#' # Age distribution (analysis 103) per site
#' age_dist <- ds.omop.achilles.distribution(analysis_ids = 103)
#' age_dist$per_site[["server_a"]]
#'
#' # Multiple distribution analyses
#' dists <- ds.omop.achilles.distribution(c(103, 105), scope = "per_site")
#' }
#' @export
ds.omop.achilles.distribution <- function(analysis_ids,
                                            scope = c("per_site", "pooled"),
                                            pooling_policy = "strict",
                                            symbol = "omop", conns = NULL,
                                            execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.achilles.distribution",
    analysis_ids = analysis_ids,
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
                as.integer(analysis_ids))
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
#' If the \code{achilles_analysis} table exists, its contents are returned
#' directly; otherwise, the server discovers available analyses by inspecting
#' distinct \code{analysis_id} values in the \code{achilles_results} table.
#' Since the catalog is identical across servers, the pooled result is taken
#' from the first responding server.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"}.
#'   The pooled result is a data frame listing all available analysis IDs
#'   and their descriptions.
#' @examples
#' \dontrun{
#' catalog <- ds.omop.achilles.catalog()
#' head(catalog$pooled)
#' }
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
