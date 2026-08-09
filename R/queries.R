# Module: Query Library (DEPRECATED)
# The curated SQL query templates are now one of the three surfaces folded into
# the unified analysis catalog (see analysis.R / dsOMOP/R/analysis_catalog.R).
# These functions are retained only as thin deprecated shims that forward to the
# ds.omop.analysis.* equivalents so existing code keeps working; new code should
# call ds.omop.analysis.list / .get / .run directly.

#' Map a legacy query id to its analysis-catalog entry name
#'
#' QueryLibrary templates are registered in the unified catalog under the
#' pack-prefixed name \code{"dsomop:<query_id>"}. This prefixes a bare legacy
#' \code{query_id} (and leaves an already-prefixed name untouched).
#' @param query_id Character; a legacy query id or an already-prefixed name.
#' @return Character; the catalog entry name.
#' @keywords internal
.query_id_to_name <- function(query_id) {
  query_id <- as.character(query_id)[[1]]
  if (grepl(":", query_id, fixed = TRUE)) query_id else paste0("dsomop:", query_id)
}

#' List available query templates (DEPRECATED)
#'
#' Deprecated shim for \code{\link{ds.omop.analysis.list}}. The curated query
#' templates are now part of the unified analysis catalog; this forwards to
#' \code{ds.omop.analysis.list()} and returns the catalog data frame.
#'
#' @param domain Character; optional domain filter. \code{NULL} (the default)
#'   returns all domains.
#' @param provider Character; ignored (retained for back-compatibility).
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Data frame of analysis-catalog entry metadata (the pooled view).
#' @examples
#' \dontrun{
#' templates <- ds.omop.query.list()
#' head(templates)
#' }
#' @seealso \code{\link{ds.omop.analysis.list}}
#' @export
ds.omop.query.list <- function(domain = NULL, provider = "native",
                                  symbol = "omop", conns = NULL) {
  .Deprecated("ds.omop.analysis.list")
  res <- ds.omop.analysis.list(domain = domain, symbol = symbol, conns = conns)
  if (is.data.frame(res$pooled)) return(res$pooled)
  data.frame()
}

#' Get query template details (DEPRECATED)
#'
#' Deprecated shim for \code{\link{ds.omop.analysis.get}}. Forwards to
#' \code{ds.omop.analysis.get()} using the entry's pack-prefixed catalog name
#' (\code{"dsomop:<query_id>"}) and returns the entry metadata list.
#'
#' @param query_id Character; the legacy query ID (e.g.,
#'   \code{"condition_prevalence"}).
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Named list of catalog entry metadata, or \code{NULL} if not found.
#' @examples
#' \dontrun{
#' meta <- ds.omop.query.get("condition_prevalence")
#' meta$params
#' }
#' @seealso \code{\link{ds.omop.analysis.get}}
#' @export
ds.omop.query.get <- function(query_id, symbol = "omop", conns = NULL) {
  .Deprecated("ds.omop.analysis.get")
  res <- ds.omop.analysis.get(.query_id_to_name(query_id),
                              symbol = symbol, conns = conns)
  res$pooled
}

#' Execute a query template (DEPRECATED)
#'
#' Deprecated shim for \code{\link{ds.omop.analysis.run}}. Forwards to
#' \code{ds.omop.analysis.run()} using the entry's pack-prefixed catalog name.
#' For back-compatibility, \code{"aggregate"} mode returns a named list of
#' per-server data frames. The legacy caller-selected \code{"assign"} mode is
#' rejected: whether an analysis is an assign loader is server-owned catalog
#' metadata and must not be asserted by the client. Use
#' \code{\link{ds.omop.analysis.run}} directly for catalog-managed loaders.
#' Disclosure controls and cross-server pooling are handled by that path.
#'
#' @param query_id Character; the legacy query ID (e.g.,
#'   \code{"condition_prevalence"}).
#' @param inputs Named list; parameter values for the entry. Default: empty list.
#' @param mode Character; only \code{"aggregate"} is accepted. The deprecated
#'   \code{"assign"} value now fails closed; assign behavior is selected from
#'   trusted server catalog metadata by \code{\link{ds.omop.analysis.run}}.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return A named list of per-server disclosure-controlled data frames. Use
#'   \code{\link{ds.omop.query.pool}} to combine them, or prefer the pooled view
#'   returned by \code{\link{ds.omop.analysis.run}} directly.
#' @examples
#' \dontrun{
#' results <- ds.omop.query.exec("condition_prevalence",
#'   inputs = list(concept_id = 201826))
#' pooled <- ds.omop.query.pool(results, query_id = "condition_prevalence")
#' }
#' @seealso \code{\link{ds.omop.analysis.run}}
#' @export
ds.omop.query.exec <- function(query_id, inputs = list(),
                                  mode = "aggregate",
                                  symbol = "omop", conns = NULL) {
  .Deprecated("ds.omop.analysis.run")
  mode <- match.arg(mode, c("aggregate", "assign"))
  if (!identical(mode, "aggregate")) {
    stop("Legacy mode='assign' is no longer accepted. Assign loaders are ",
         "selected from trusted server catalog metadata by ",
         "ds.omop.analysis.run().", call. = FALSE)
  }
  res <- ds.omop.analysis.run(.query_id_to_name(query_id), params = inputs,
                              symbol = symbol, conns = conns, type = "split")
  out <- res$per_site
  attr(out, "dsomop.pooling_contract") <- res$meta$pooling_contract
  attr(out, "dsomop.harmonization") <- res$meta$harmonization
  attr(out, "dsomop.analysis_name") <- .query_id_to_name(query_id)
  attr(out, "dsomop.expected_servers") <- res$meta$servers
  out
}

#' Pool query template results across servers
#'
#' Takes per-server results from \code{\link{ds.omop.query.exec}} and delegates
#' to the exact server-owned pooling contract carried by that result. It never
#' infers keys, count fields, rates, or statistics from column names.
#'
#' Suppression-safe pooling policy: if any server suppressed a cell (marked
#' as NA by the server's disclosure controls), the corresponding pooled cell
#' also becomes NA under \code{"strict"} policy. This prevents reconstructing
#' small-site counts by subtracting the pooled total from known large sites.
#'
#' @param results Named list of per-server data frames, as returned by
#'   \code{\link{ds.omop.query.exec}}.
#' @param query_id Optional character query ID; when supplied, it must match the
#'   analysis identity carried by \code{results}.
#' @param sensitive_fields Deprecated and unsupported; must remain \code{NULL}.
#' @param pool_strategy Deprecated and unsupported; must remain \code{"sum"}.
#' @param policy Character; suppression propagation policy. \code{"strict"}
#'   (the default) sets the pooled value to NA if any server suppressed the
#'   cell. \code{"pooled_only_ok"} sums only the non-suppressed values.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @return A data frame with contractually pooled results, or \code{NULL} when
#'   the contracted estimand has no combinable result.
#' @section Deprecated:
#' New code should use \code{\link{ds.omop.analysis.run}}, whose \code{pooled}
#' element already contains the cross-server aggregation. This helper remains
#' only as a contractual bridge for results from deprecated
#' \code{\link{ds.omop.query.exec}}.
#' @examples
#' \dontrun{
#' results <- ds.omop.query.exec("condition_prevalence")
#' pooled <- ds.omop.query.pool(results, query_id = "condition_prevalence")
#' pooled
#' }
#' @seealso \code{\link{ds.omop.analysis.run}}
#' @export
ds.omop.query.pool <- function(results, query_id = NULL,
                                  sensitive_fields = NULL,
                                  pool_strategy = "sum",
                                  policy = "strict",
                                  symbol = "omop") {
  .Deprecated("ds.omop.analysis.run")
  policy <- match.arg(policy, c("strict", "pooled_only_ok"))
  if (is.null(results) || length(results) == 0) return(NULL)
  if (!is.null(sensitive_fields) || !identical(pool_strategy, "sum")) {
    stop("Caller-selected pooling fields or strategies are no longer accepted. ",
         "Use ds.omop.analysis.run(), whose server-owned contract defines the ",
         "exact pooling algebra.", call. = FALSE)
  }
  if (length(attr(results, "ds_errors") %||% list()) > 0L) {
    stop("Cannot pool an incomplete federated query result.", call. = FALSE)
  }
  expected <- attr(results, "dsomop.expected_servers")
  if (!is.character(expected) || !setequal(names(results), expected)) {
    stop("Query results do not prove complete federation coverage; rerun with ",
         "ds.omop.analysis.run().", call. = FALSE)
  }
  analysis_name <- attr(results, "dsomop.analysis_name")
  if (!is.null(query_id) &&
      !identical(.query_id_to_name(query_id), analysis_name)) {
    stop("query_id does not match the server-owned result contract.",
         call. = FALSE)
  }
  contract <- attr(results, "dsomop.pooling_contract")
  harmonization <- attr(results, "dsomop.harmonization")
  if (is.null(contract)) {
    stop("Query results carry no server-owned pooling contract; rerun with ",
         "ds.omop.analysis.run().", call. = FALSE)
  }
  .pool_analysis_contract(
    results, contract, policy = policy, harmonization = harmonization
  )$result
}
