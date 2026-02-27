# ==============================================================================
# dsOMOPClient v2 - Query Library Client Wrappers + Pooling
# ==============================================================================
# Client-side functions for listing, executing, and pooling query templates
# from the dsOMOP query template repository.
# ==============================================================================

#' List available query templates
#'
#' Returns metadata for all available query templates on connected servers.
#' Queries are classified as SAFE_AGGREGATE, SAFE_ASSIGN, or BLOCKED.
#' Only non-BLOCKED queries are returned.
#'
#' @param domain Character; optional domain filter (e.g., "Condition", "Drug")
#' @param provider Character; query provider ("native" or "all")
#' @param symbol Character; OMOP session symbol (default "omop")
#' @param conns DSI connections (default: from session)
#' @return Data frame with query metadata (id, group, name, description,
#'   mode, class, poolable, cdm_version, n_inputs)
#' @export
ds.omop.query.list <- function(domain = NULL, provider = "native",
                                  symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  results <- DSI::datashield.aggregate(
    conns,
    expr = call("omopQueryListDS", session$res_symbol,
                domain, provider)
  )

  # Return first server's result (metadata is identical across servers)
  if (length(results) > 0) {
    result <- results[[1]]
    if (is.data.frame(result)) return(result)
  }

  data.frame(
    id = character(0), group = character(0), name = character(0),
    description = character(0), mode = character(0),
    class = character(0), poolable = logical(0),
    cdm_version = character(0), n_inputs = integer(0),
    stringsAsFactors = FALSE
  )
}

#' Get query template details
#'
#' Returns full metadata for a specific query template, including input
#' parameters, output schema, and sensitive field annotations.
#'
#' @param query_id Character; the query ID from the query library
#' @param symbol Character; OMOP session symbol (default "omop")
#' @param conns DSI connections (default: from session)
#' @return Named list with query metadata
#' @export
ds.omop.query.get <- function(query_id, symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  results <- DSI::datashield.aggregate(
    conns,
    expr = call("omopQueryGetDS", session$res_symbol, query_id)
  )

  if (length(results) > 0) return(results[[1]])
  NULL
}

#' Execute a query template
#'
#' Executes a query template template against connected servers with
#' DataSHIELD-aligned disclosure controls. Returns per-server results
#' (for aggregate mode) or TRUE (for assign mode).
#'
#' @param query_id Character; the query ID from the query library
#' @param inputs Named list; parameter values for the query template
#' @param mode Character; "aggregate" (default) or "assign"
#' @param symbol Character; OMOP session symbol (default "omop")
#' @param conns DSI connections (default: from session)
#' @return For aggregate mode: named list of per-server data frames.
#'   For assign mode: TRUE.
#' @export
ds.omop.query.exec <- function(query_id, inputs = list(),
                                  mode = "aggregate",
                                  symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  mode <- match.arg(mode, c("aggregate", "assign"))

  if (mode == "aggregate") {
    results <- DSI::datashield.aggregate(
      conns,
      expr = call("omopQueryExecDS", session$res_symbol,
                   query_id, inputs, mode)
    )
    return(results)
  }

  # Assign mode
  DSI::datashield.assign.expr(
    conns,
    symbol = paste0("query_", gsub("\\.", "_", query_id)),
    expr = call("omopQueryExecDS", session$res_symbol,
                query_id, inputs, mode)
  )
  invisible(TRUE)
}

#' Pool query template results across servers
#'
#' Takes per-server results from \code{ds.omop.query.exec()} and safely
#' pools them according to the query's pool strategy. Supports:
#' \itemize{
#'   \item \code{sum}: Sum count columns across servers
#'   \item \code{weighted_mean}: Pool means weighted by count
#' }
#'
#' Pooling policy:
#' \itemize{
#'   \item If any server suppressed a cell (NA), the pooled cell becomes NA
#'   \item This prevents "pooling to unmask" small-site counts
#' }
#'
#' @param results Named list of per-server data frames (from
#'   \code{ds.omop.query.exec()})
#' @param query_id Character; query ID (to look up pool strategy)
#' @param sensitive_fields Character vector; columns to apply suppression
#'   pooling rules to. If NULL, auto-detected from query metadata.
#' @param pool_strategy Character; "sum" (default), "weighted_mean", or "none"
#' @param policy Character; "strict" (NA if any site suppressed) or
#'   "pooled_only_ok" (pool what's available)
#' @param symbol Character; OMOP session symbol
#' @return Data frame with pooled results, or NULL if pooling failed
#' @export
ds.omop.query.pool <- function(results, query_id = NULL,
                                  sensitive_fields = NULL,
                                  pool_strategy = "sum",
                                  policy = "strict",
                                  symbol = "omop") {
  if (is.null(results) || length(results) == 0) return(NULL)

  # Filter out non-data.frame results (errors)
  valid <- Filter(function(r) is.data.frame(r) && nrow(r) > 0, results)
  if (length(valid) == 0) return(NULL)

  # Single server: return as-is
  if (length(valid) == 1) return(valid[[1]])

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
