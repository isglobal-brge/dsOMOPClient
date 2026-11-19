# Module: Achilles Analytics
# Client-side wrappers for querying pre-computed Achilles aggregate statistics.

.canonical_achilles_catalog <- function(x) {
  if (!is.data.frame(x)) return(NULL)
  x <- unique(x[, sort(names(x)), drop = FALSE])
  if (nrow(x) > 1L && ncol(x) > 0L) {
    ordering_columns <- lapply(x, function(column) {
      if (is.factor(column)) as.character(column) else column
    })
    ordering <- tryCatch(
      do.call(order, c(ordering_columns, list(na.last = TRUE))),
      error = function(e) NULL
    )
    if (is.null(ordering)) return(NULL)
    x <- x[ordering, , drop = FALSE]
  }
  rownames(x) <- NULL
  x
}

.pool_achilles_catalog <- function(raw, label) {
  aggregate_errors <- attr(raw, "ds_errors") %||% list()
  if (length(aggregate_errors) > 0L) {
    return(list(
      result = NULL,
      warnings = paste0(
        "Federated ", label, " unavailable: incomplete federation; ",
        "unavailable server(s): ",
        paste(names(aggregate_errors), collapse = ", "), "."
      )
    ))
  }
  if (length(raw) == 0L) {
    return(list(result = NULL,
                warnings = paste0("No ", label, " metadata returned.")))
  }

  canonical <- lapply(raw, .canonical_achilles_catalog)
  valid <- !vapply(canonical, is.null, logical(1L))
  if (!all(valid)) {
    return(list(
      result = NULL,
      warnings = paste0(
        "Federated ", label, " unavailable: invalid metadata from server(s): ",
        paste(names(raw)[!valid], collapse = ", "), "."
      )
    ))
  }
  reference <- canonical[[1L]]
  consistent <- vapply(canonical, identical, logical(1L), reference)
  if (!all(consistent)) {
    return(list(
      result = NULL,
      warnings = paste0(
        "Federated ", label, " unavailable: server catalogs differ: ",
        paste(names(raw)[!consistent], collapse = ", "), "."
      )
    ))
  }

  list(result = raw[[1L]], warnings = character(0))
}

#' Check Achilles availability
#'
#' Queries each connected server to determine whether Achilles result tables
#' (\code{achilles_results} and \code{achilles_results_dist}) are present
#' and accessible. Returns per-site availability status and table row counts.
#' No pooling is performed because this is metadata-only.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param type Result view: \code{"split"} (default) or \code{"both"}.
#'   Status is site-specific: \code{"both"} returns \code{pooled = NULL} with
#'   \code{meta$pooling_reason}, and \code{"combine"} stops before contacting
#'   the servers. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result} object (per-server view).
#'   Each server's result is a list or data frame indicating whether the
#'   Achilles tables exist and how many rows they contain.
#' @examples
#' \dontrun{
#' status <- ds.omop.achilles.status()
#' status$per_site[["server_a"]]$available
#' }
#' @export
ds.omop.achilles.status <- function(symbol = "omop", conns = NULL,
                                    type = "split",
                                    datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.achilles.status")
  result_type <- .resolve_result_type(type, default_type = "split")
  reason <- paste0("Achilles availability status is site-specific and ",
                   "cannot be combined.")
  .combine_unavailable(result_type, reason)
  code <- .build_code("ds.omop.achilles.status", symbol = symbol,
                      type = result_type)

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAchillesStatusDS", session$res_symbol)
  )

  .result_type_view(dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code)), result_type, reason)
}

#' List available Achilles analyses
#'
#' Returns the catalog of Achilles analyses available on the connected servers,
#' optionally filtered by clinical domain. The catalog is defined by the
#' Achilles specification, so the pooled result is the catalog every server
#' returned identically; when the servers' catalogs differ, or a server is
#' unavailable, \code{$pooled} is \code{NULL} with
#' \code{meta$pooling_reason}.
#'
#' @param domain Character; optional domain filter. Valid values include
#'   \code{"person"}, \code{"condition"}, \code{"drug"}, \code{"measurement"},
#'   \code{"procedure"}, \code{"observation"}, \code{"visit"}, and
#'   \code{"device"}. \code{NULL} (the default) returns all domains.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. Each \code{$per_site} entry and the
#'   pooled consensus are data frames listing analysis IDs, names, and
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
                                       conns = NULL, type = "both",
                                       datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.achilles.analyses")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.achilles.analyses",
    domain = domain, symbol = symbol, type = result_type)

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAchillesAnalysesDS", session$res_symbol, domain)
  )

  catalog <- if (.result_type_wants_combine(result_type)) {
    .pool_achilles_catalog(raw, "Achilles analyses catalog")
  } else {
    list(result = NULL, warnings = character(0))
  }

  .result_type_view(dsomop_result(
    per_site = raw, pooled = catalog$result,
    meta = list(call_code = code, warnings = catalog$warnings)), result_type,
    "No complete, consistent Achilles analyses catalog was available.",
    pooling = .pooling_meta("metadata_consensus"))
}

#' Get Achilles count results
#'
#' Queries the \code{achilles_results} table on each connected server for
#' the specified analysis IDs. These are pre-computed count-based statistics
#' (e.g., person counts by gender, condition frequency). By default (as
#' \code{ds.table} does) the result has both views: counts are summed across
#' servers with suppression propagation, and a stratum suppressed or absent on
#' any server is left out of the pooled table.
#'
#' @param analysis_ids Integer vector; the Achilles analysis IDs to retrieve
#'   (e.g., \code{c(1, 2, 400)} for total persons, gender breakdown, and
#'   condition frequency).
#' @param scope Deprecated alias of \code{type} (\code{"per_site"} or
#'   \code{"pooled"}).
#' @param pooling_policy Character; \code{"strict"} (the default) leaves out a
#'   stratum absent on any server; \code{"pooled_only_ok"} sums the strata the
#'   servers returned. A stratum suppressed (NA) on any server is left out
#'   under both policies. See \link{dsomop_conventions}.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result} object. The \code{per_site} element contains
#'   per-server data frames with columns \code{analysis_id}, \code{stratum_1}
#'   through \code{stratum_5}, and \code{count_value}. The \code{pooled}
#'   element (if requested) contains the aggregated data frame.
#' @examples
#' \dontrun{
#' # Total person count (analysis 1) per server and pooled
#' res <- ds.omop.achilles.results(analysis_ids = 1)
#' res$pooled
#'
#' # Gender breakdown per site only
#' gender <- ds.omop.achilles.results(analysis_ids = 2, type = "split")
#' gender$per_site
#' }
#' @export
ds.omop.achilles.results <- function(analysis_ids,
                                      scope = c("per_site", "pooled"),
                                      pooling_policy = "strict",
                                      symbol = "omop", conns = NULL,
                                      execute = TRUE, type = "both",
                                      datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.achilles.results")
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing,
    type_missing = missing(type), default_type = "both",
    fn = "ds.omop.achilles.results"
  )
  pooling_policy <- .pooling_policy_arg(pooling_policy)

  code <- .build_code("ds.omop.achilles.results",
    analysis_ids = analysis_ids,
    pooling_policy = .code_policy(pooling_policy),
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAchillesResultsDS", session$res_symbol,
                .ds_encode(as.integer(analysis_ids)))
  )

  .reader_result(raw, "achilles_results", result_type, pooling_policy, code,
    "No complete disclosure-safe pooled Achilles count result was available.")
}

#' Get Achilles distribution results
#'
#' Queries the \code{achilles_results_dist} table on each connected server
#' for the specified distribution analysis IDs (e.g., age distribution,
#' observation period length). Distribution analyses store percentile values
#' (p10, p25, median, p75, p90) rather than raw counts. Extreme values
#' (min/max) are never returned by the server for disclosure protection.
#'
#' Like \code{ds.mean}, the default view is per server. A pooled view
#' (\code{type = "both"}) combines the counts, count-weighted averages and
#' pooled SDs; percentile values cannot be combined from pre-computed
#' summaries, so all percentile columns in the pooled result are \code{NA}.
#' Use per-site data for percentile display.
#'
#' @param analysis_ids Integer vector; distribution analysis IDs to retrieve
#'   (e.g., \code{c(103, 105)} for age and observation length distributions).
#' @param scope Deprecated alias of \code{type} (\code{"per_site"} or
#'   \code{"pooled"}).
#' @param pooling_policy Character; \code{"strict"} (the default) or
#'   \code{"pooled_only_ok"}. See \link{dsomop_conventions}.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"split"} (default), \code{"both"}, or
#'   \code{"combine"}. See \link{dsomop_conventions}.
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
#' # Multiple distribution analyses, pooled as well
#' dists <- ds.omop.achilles.distribution(c(103, 105), type = "both")
#' }
#' @export
ds.omop.achilles.distribution <- function(analysis_ids,
                                            scope = c("per_site", "pooled"),
                                            pooling_policy = "strict",
                                            symbol = "omop", conns = NULL,
                                            execute = TRUE, type = "split",
                                            datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.achilles.distribution")
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing,
    type_missing = missing(type), default_type = "split",
    fn = "ds.omop.achilles.distribution"
  )
  pooling_policy <- .pooling_policy_arg(pooling_policy)

  code <- .build_code("ds.omop.achilles.distribution",
    analysis_ids = analysis_ids,
    pooling_policy = .code_policy(pooling_policy),
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAchillesDistributionDS", session$res_symbol,
                .ds_encode(as.integer(analysis_ids)))
  )

  .reader_result(raw, "achilles_distribution", result_type, pooling_policy,
    code,
    "No complete disclosure-safe pooled Achilles distribution was available.")
}

#' Get Achilles analysis catalog
#'
#' Returns the full catalog of available Achilles analyses from the server.
#' If the \code{achilles_analysis} table exists, its contents are returned
#' directly; otherwise, the server discovers available analyses by inspecting
#' distinct \code{analysis_id} values in the \code{achilles_results} table.
#' The pooled result is the catalog every server returned identically; when
#' the servers' catalogs differ, or a server is unavailable, \code{$pooled}
#' is \code{NULL} with \code{meta$pooling_reason}.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. Each \code{$per_site} entry and the
#'   pooled consensus are data frames listing the available analysis IDs and
#'   their descriptions.
#' @examples
#' \dontrun{
#' catalog <- ds.omop.achilles.catalog()
#' head(catalog$pooled)
#' }
#' @export
ds.omop.achilles.catalog <- function(symbol = "omop", conns = NULL,
                                     type = "both",
                                     datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.achilles.catalog")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.achilles.catalog", symbol = symbol,
                      type = result_type)

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAchillesCatalogDS", session$res_symbol)
  )

  catalog <- if (.result_type_wants_combine(result_type)) {
    .pool_achilles_catalog(raw, "Achilles analysis catalog")
  } else {
    list(result = NULL, warnings = character(0))
  }

  .result_type_view(dsomop_result(
    per_site = raw, pooled = catalog$result,
    meta = list(call_code = code, warnings = catalog$warnings)), result_type,
    "No complete, consistent Achilles analysis catalog was available.",
    pooling = .pooling_meta("metadata_consensus"))
}

#' Get Achilles Heel data-quality warnings (per site)
#'
#' Achilles Heel counts records and does not provide distinct-person support for
#' each fired rule. It therefore cannot satisfy dsOMOP's person-level
#' contribution contract and is no longer available through DataSHIELD. Run it
#' only in a controller-side quality-assurance workflow. This function always
#' stops with that explanation, without contacting any server; it is kept so
#' existing code fails with a clear reason.
#'
#' @param symbol Character; session symbol (default "omop"). Unused.
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @return Nothing: the function always stops with an error.
#' @export
ds.omop.achilles.heel <- function(symbol = "omop", conns = NULL,
                                  datasources = NULL) {
  stop("Achilles Heel is controller-only: its record counts have no reviewed ",
       "distinct-person contribution contract.", call. = FALSE)
}
