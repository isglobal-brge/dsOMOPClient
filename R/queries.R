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
                              symbol = symbol, conns = conns)
  res$per_site
}

#' Pool query template results across servers
#'
#' Takes per-server results from \code{\link{ds.omop.query.exec}} and safely
#' pools them according to the query's pool strategy. Supports two pooling
#' methods:
#' \itemize{
#'   \item \code{sum}: Sum count columns across servers (default)
#'   \item \code{weighted_mean}: legacy metadata value; rejected because the
#'     deprecated helper has no closed numerator/denominator contract
#' }
#'
#' Suppression-safe pooling policy: if any server suppressed a cell (marked
#' as NA by the server's disclosure controls), the corresponding pooled cell
#' also becomes NA under \code{"strict"} policy. This prevents reconstructing
#' small-site counts by subtracting the pooled total from known large sites.
#'
#' The function auto-detects sensitive (count-like) columns from column names
#' matching patterns such as \code{n_*}, \code{*_count}, \code{count_value},
#' and \code{num_*}. Non-sensitive columns are used as join keys for
#' cross-server merging.
#'
#' @param results Named list of per-server data frames, as returned by
#'   \code{\link{ds.omop.query.exec}}.
#' @param query_id Character; query ID used to look up the recommended pool
#'   strategy and sensitive field annotations from the query metadata. If
#'   \code{NULL}, the function falls back to auto-detection.
#' @param sensitive_fields Character vector; column names to apply suppression
#'   pooling rules to. If \code{NULL} (the default), auto-detected from
#'   query metadata or column name patterns.
#' @param pool_strategy Character; pooling method. \code{"sum"} (the default)
#'   sums count columns across servers; \code{"none"} returns the first server's
#'   result without pooling. The legacy \code{"weighted_mean"} value fails
#'   closed; use \code{ds.omop.analysis.run()} for contracted pooling.
#' @param policy Character; suppression propagation policy. \code{"strict"}
#'   (the default) sets the pooled value to NA if any server suppressed the
#'   cell. \code{"pooled_only_ok"} sums only the non-suppressed values.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @return A data frame with pooled results, or \code{NULL} if pooling
#'   failed (no valid data frames, all empty, etc.). For a single server,
#'   its result is returned as-is.
#' @section Deprecated:
#' New code should use \code{\link{ds.omop.analysis.run}}, whose \code{pooled}
#' element already contains the cross-server aggregation. This client-side pooler
#' is retained for back-compatibility with results produced by the deprecated
#' \code{\link{ds.omop.query.exec}}.
#' @examples
#' \dontrun{
#' results <- ds.omop.query.exec("condition_prevalence")
#' pooled <- ds.omop.query.pool(results, query_id = "condition_prevalence")
#' pooled
#'
#' # Manual sensitive field specification
#' pooled <- ds.omop.query.pool(results,
#'   sensitive_fields = c("n_persons", "n_records"),
#'   policy = "strict")
#' }
#' @seealso \code{\link{ds.omop.analysis.run}}
#' @export
ds.omop.query.pool <- function(results, query_id = NULL,
                                  sensitive_fields = NULL,
                                  pool_strategy = "sum",
                                  policy = "strict",
                                  symbol = "omop") {
  .Deprecated("ds.omop.analysis.run")
  pool_strategy <- match.arg(pool_strategy,
                             c("sum", "weighted_mean", "none"))
  policy <- match.arg(policy, c("strict", "pooled_only_ok"))
  if (is.null(results) || length(results) == 0) return(NULL)

  # A strict pool must never silently become a partial federation.  Preserve
  # the permissive legacy behaviour only when the caller explicitly requests
  # pooled_only_ok.
  result_errors <- attr(results, "ds_errors") %||% list()
  valid_mask <- vapply(results, function(r) {
    is.data.frame(r) && nrow(r) > 0L
  }, logical(1))
  if (identical(policy, "strict") &&
      (length(result_errors) > 0L || any(!valid_mask))) {
    return(NULL)
  }

  # Filter out non-data.frame/empty results under pooled_only_ok.
  valid <- Filter(function(r) is.data.frame(r) && nrow(r) > 0, results)
  if (length(valid) == 0) return(NULL)

  # Try to get pool strategy from query metadata
  if (!is.null(query_id) && is.null(sensitive_fields)) {
    tryCatch({
      meta <- ds.omop.query.get(query_id, symbol = symbol)
      if (!is.null(meta$sensitive_fields)) {
        sensitive_fields <- meta$sensitive_fields
      }
      if (!is.null(meta$pool_strategy)) {
        pool_strategy <- meta$pool_strategy
      }
    }, error = function(e) NULL)
  }
  pool_strategy <- match.arg(pool_strategy,
                             c("sum", "weighted_mean", "none"))
  if (identical(pool_strategy, "weighted_mean")) {
    stop("The deprecated query pooler cannot safely infer a weighted-mean ",
         "contract. Use ds.omop.analysis.run() instead.", call. = FALSE)
  }

  # Single complete/contributing server: no cross-server arithmetic remains,
  # but only after every caller-selected/metadata strategy has been validated.
  if (length(valid) == 1L) return(valid[[1L]])

  # Auto-detect sensitive fields from column names
  if (is.null(sensitive_fields)) {
    all_cols <- names(valid[[1]])
    sensitive_fields <- all_cols[grepl(
      "^n_|_count$|count_value|num_|n$",
      all_cols, ignore.case = TRUE
    )]
  }

  if (pool_strategy == "none") {
    return(valid[[1]])
  }

  # Back-compatibility must not bypass the federated release contract.  When
  # this deprecated helper is used with a live OMOP session, require identical
  # count-band widths before any client-side sum/weighted pool.
  session <- tryCatch(.get_session(symbol), error = function(e) NULL)
  if (!is.null(session)) {
    .session_harmonization_for_connections(
      session, session$conns, require_count_pooling = TRUE)
  }

  # Determine join key columns (non-sensitive, non-numeric)
  all_cols <- names(valid[[1]])
  key_cols <- setdiff(all_cols, sensitive_fields)
  # Keep only columns that look like keys (character or integer IDs)
  numeric_cols <- all_cols[vapply(valid[[1]], is.numeric, logical(1))]
  key_cols <- setdiff(key_cols, setdiff(numeric_cols, grep("_id$", numeric_cols, value = TRUE)))

  if (length(key_cols) == 0) {
    # No join keys: just sum the entire data frames
    pooled <- valid[[1]]
    for (i in seq_along(valid)[-1]) {
      for (col in sensitive_fields) {
        if (col %in% names(pooled) && col %in% names(valid[[i]])) {
          pooled[[col]] <- .pool_col(pooled[[col]], valid[[i]][[col]], policy)
        }
      }
    }
    return(pooled)
  }

  # Join on key columns and pool sensitive fields
  pooled <- valid[[1]]
  for (i in seq_along(valid)[-1]) {
    site_df <- valid[[i]]

    # Merge on common key columns
    common_keys <- intersect(key_cols, names(site_df))
    if (length(common_keys) == 0) next

    merged <- merge(pooled, site_df, by = common_keys,
                    all = TRUE, suffixes = c(".pool", ".site"))

    # Pool sensitive columns
    for (col in sensitive_fields) {
      pool_col <- paste0(col, ".pool")
      site_col <- paste0(col, ".site")

      if (pool_col %in% names(merged) && site_col %in% names(merged)) {
        merged[[col]] <- .pool_col(merged[[pool_col]], merged[[site_col]], policy)
        merged[[pool_col]] <- NULL
        merged[[site_col]] <- NULL
      } else if (col %in% names(merged)) {
        # Column exists without suffix (no conflict during merge)
      }
    }

    # Clean up any remaining suffix columns
    suffix_cols <- grep("\\.(pool|site)$", names(merged), value = TRUE)
    for (sc in suffix_cols) {
      base_name <- sub("\\.(pool|site)$", "", sc)
      if (!base_name %in% names(merged)) {
        names(merged)[names(merged) == sc] <- base_name
      }
    }
    # Remove duplicated suffix columns
    merged <- merged[, !grepl("\\.(pool|site)$", names(merged)), drop = FALSE]

    pooled <- merged
  }

  # Reorder columns to match original
  original_order <- intersect(all_cols, names(pooled))
  extra_cols <- setdiff(names(pooled), all_cols)
  pooled <- pooled[, c(original_order, extra_cols), drop = FALSE]

  pooled
}

#' Pool a single column across two data frames
#' @keywords internal
.pool_col <- function(a, b, policy = "strict") {
  policy <- match.arg(policy, c("strict", "pooled_only_ok"))
  if (policy == "strict") {
    # If either is NA (suppressed), result is NA
    result <- ifelse(is.na(a) | is.na(b), NA_real_, a + b)
  } else {
    # pooled_only_ok: sum what's available
    a[is.na(a)] <- 0
    b[is.na(b)] <- 0
    result <- a + b
  }
  result
}
