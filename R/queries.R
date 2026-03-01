# Module: Query Library
# Client-side wrappers for listing, inspecting, and executing curated SQL
# query templates from the dsOMOP query library.

#' List available query templates
#'
#' Returns metadata for all available query templates on connected servers.
#' Templates are classified by their DataSHIELD security mode:
#' \code{SAFE_AGGREGATE} (returns aggregate results to the client),
#' \code{SAFE_ASSIGN} (stores results server-side), or \code{BLOCKED}
#' (not permitted). Only non-BLOCKED templates are returned. Since the
#' query library is defined by the server package, the catalog is identical
#' across servers and the first server's result is returned.
#'
#' @param domain Character; optional domain filter to restrict results
#'   (e.g., \code{"Condition"}, \code{"Drug"}, \code{"Measurement"}).
#'   \code{NULL} (the default) returns templates for all domains.
#' @param provider Character; query provider filter. \code{"native"} (the
#'   default) returns only built-in dsOMOP templates; \code{"all"} includes
#'   any additional registered providers.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Data frame with query metadata columns: \code{id}, \code{group},
#'   \code{name}, \code{description}, \code{mode}, \code{class},
#'   \code{poolable}, \code{cdm_version}, \code{n_inputs}. Returns an empty
#'   data frame with the correct schema if no templates match.
#' @examples
#' \dontrun{
#' # List all available templates
#' templates <- ds.omop.query.list()
#' head(templates)
#'
#' # List only drug-related templates
#' drug_queries <- ds.omop.query.list(domain = "Drug")
#' }
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
#' Returns the full metadata for a specific query template, including its
#' input parameter definitions, output column schema, sensitive field
#' annotations (used by disclosure controls), and the recommended pooling
#' strategy. This is useful for programmatically building input forms or
#' understanding what a query will return before executing it.
#'
#' @param query_id Character; the unique query ID from the query library
#'   (e.g., \code{"condition_prevalence"}).
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Named list with query metadata, including \code{id}, \code{name},
#'   \code{description}, \code{inputs}, \code{output_schema},
#'   \code{sensitive_fields}, and \code{pool_strategy}. Returns \code{NULL}
#'   if the query ID is not found.
#' @examples
#' \dontrun{
#' meta <- ds.omop.query.get("condition_prevalence")
#' meta$inputs
#' meta$sensitive_fields
#' }
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
#' Executes a query template against all connected servers with
#' DataSHIELD-aligned disclosure controls applied server-side. In
#' \code{"aggregate"} mode, results are returned to the client as per-server
#' data frames. In \code{"assign"} mode, results are stored server-side
#' under a generated symbol name and the function returns \code{TRUE}
#' invisibly.
#'
#' Disclosure controls (small-count suppression) are applied by the server
#' before results leave the server. Rows with counts below the server's
#' configured threshold are either dropped or have their counts set to NA.
#'
#' @param query_id Character; the unique query ID from the query library
#'   (e.g., \code{"condition_prevalence"}).
#' @param inputs Named list; parameter values required by the query template.
#'   Use \code{\link{ds.omop.query.get}} to discover required inputs and
#'   their types. Default: empty list (for queries with no required inputs).
#' @param mode Character; \code{"aggregate"} (the default) returns results
#'   to the client, \code{"assign"} stores results server-side for further
#'   analysis.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return For \code{mode = "aggregate"}: a named list of per-server data
#'   frames. For \code{mode = "assign"}: \code{TRUE} invisibly. Use
#'   \code{\link{ds.omop.query.pool}} to combine aggregate results.
#' @examples
#' \dontrun{
#' # Execute a query with no inputs
#' results <- ds.omop.query.exec("gender_distribution")
#' results[["server_a"]]
#'
#' # Execute a query with inputs and pool the results
#' results <- ds.omop.query.exec("condition_prevalence",
#'   inputs = list(concept_id = 201826))
#' pooled <- ds.omop.query.pool(results, query_id = "condition_prevalence")
#' }
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
#' Takes per-server results from \code{\link{ds.omop.query.exec}} and safely
#' pools them according to the query's pool strategy. Supports two pooling
#' methods:
#' \itemize{
#'   \item \code{sum}: Sum count columns across servers (default)
#'   \item \code{weighted_mean}: Pool means weighted by count
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
#'   sums count columns across servers; \code{"weighted_mean"} pools means
#'   weighted by count; \code{"none"} returns the first server's result
#'   without pooling.
#' @param policy Character; suppression propagation policy. \code{"strict"}
#'   (the default) sets the pooled value to NA if any server suppressed the
#'   cell. \code{"pooled_only_ok"} sums only the non-suppressed values.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @return A data frame with pooled results, or \code{NULL} if pooling
#'   failed (no valid data frames, all empty, etc.). For a single server,
#'   its result is returned as-is.
#' @examples
#' \dontrun{
#' results <- ds.omop.query.exec("gender_distribution")
#' pooled <- ds.omop.query.pool(results, query_id = "gender_distribution")
#' pooled
#'
#' # Manual sensitive field specification
#' pooled <- ds.omop.query.pool(results,
#'   sensitive_fields = c("n_persons", "n_records"),
#'   policy = "strict")
#' }
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
