# Module: Data Exploration
# Client-side wrappers for OMOP CDM data profiling and exploration functions.

#' Get safe numeric cutpoints for a column
#'
#' @description
#' Computes disclosure-safe bin edges for a numeric column in an OMOP CDM
#' table. Each bin is guaranteed to contain enough persons to satisfy the
#' minimum cell count threshold, making the resulting breakpoints safe for
#' use as filter boundaries or histogram edges. The cutpoints are computed
#' server-side and returned without exposing individual-level data.
#'
#' @param table Character; the OMOP CDM table name (e.g.,
#'   \code{"measurement"}, \code{"observation"}).
#' @param column Character; the numeric column to bin (e.g.,
#'   \code{"value_as_number"}).
#' @param concept_id Integer or NULL; optional concept ID to restrict
#'   rows before computing bins (default: NULL for all rows).
#' @param n_bins Integer; the target number of bins (default: 10). The
#'   server may return fewer bins if disclosure constraints require merging.
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#'   Cutpoints are inherently per-site; pooled scope is accepted but the
#'   pooled slot will be NULL.
#' @param symbol Character; the session symbol identifying the OMOP
#'   connection (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   where each element contains a list with \code{breaks} and \code{counts}),
#'   \code{$pooled} (always NULL for cutpoints), and \code{$meta} (list with
#'   \code{call_code} and \code{scope}).
#' @examples
#' \dontrun{
#' cuts <- ds.omop.safe.cutpoints("measurement", "value_as_number",
#'                                 concept_id = 3004249, n_bins = 5)
#' cuts$per_site$server1$breaks
#' }
#' @export
ds.omop.safe.cutpoints <- function(table, column, concept_id = NULL,
                                    n_bins = 10L,
                                    scope = c("per_site", "pooled"),
                                    symbol = "omop", conns = NULL,
                                    execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.safe.cutpoints",
    table = table, column = column, concept_id = concept_id,
    n_bins = n_bins, scope = scope, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopSafeCutpointsDS", session$res_symbol,
                table, column, concept_id, as.integer(n_bins))
  )

  dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = scope))
}

#' Create a safe numeric value filter using server-computed bins
#'
#' @description
#' Convenience wrapper that first calls \code{\link{ds.omop.safe.cutpoints}}
#' to obtain disclosure-safe bin edges, then creates an
#' \code{\link{omop_filter_value}} filter whose boundary is snapped to the
#' nearest safe bin edge. This ensures that any subsequent filtering
#' operation will not inadvertently create small cells that violate
#' disclosure controls.
#'
#' @param table Character; the OMOP CDM table name (e.g.,
#'   \code{"measurement"}).
#' @param column Character; the numeric column to filter on (e.g.,
#'   \code{"value_as_number"}).
#' @param threshold Numeric; the desired threshold value. The function
#'   snaps this to the nearest safe bin boundary.
#' @param direction Character; \code{"above"} (default) or \code{"below"},
#'   indicating whether to keep values above or below the threshold.
#' @param concept_id Integer or NULL; optional concept ID to restrict
#'   rows before computing cutpoints (default: NULL).
#' @param n_bins Integer; the number of bins for cutpoint computation
#'   (default: 10).
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @return An \code{omop_filter} object of type \code{value_bin}, suitable
#'   for passing to query or extraction functions.
#' @examples
#' \dontrun{
#' filt <- ds.omop.safe.filter.value("measurement", "value_as_number",
#'                                    threshold = 100, direction = "above",
#'                                    concept_id = 3004249)
#' }
#' @export
ds.omop.safe.filter.value <- function(table, column, threshold,
                                       direction = c("above", "below"),
                                       concept_id = NULL,
                                       n_bins = 10L,
                                       symbol = "omop", conns = NULL) {
  direction <- match.arg(direction)
  cuts <- ds.omop.safe.cutpoints(table, column,
    concept_id = concept_id, n_bins = n_bins,
    scope = "per_site", symbol = symbol, conns = conns)
  # Use first server's breaks (all should be compatible)
  breaks <- NULL
  for (srv in names(cuts$per_site)) {
    srv_data <- cuts$per_site[[srv]]
    if (is.list(srv_data) && !is.null(srv_data$breaks)) {
      breaks <- srv_data$breaks
      break
    }
  }
  if (is.null(breaks)) {
    stop("Could not obtain safe cutpoints for ", table, ".", column, call. = FALSE)
  }
  omop_filter_value(column = column, threshold = threshold,
                     direction = direction,
                     safe_bins = list(breaks = breaks))
}

#' Get concept prevalence for a table
#'
#' @description
#' Retrieves the most frequent concepts in the specified OMOP CDM table,
#' ranked by person count or record count. Results are disclosure-controlled
#' on the server side (small cells are suppressed) and returned as a
#' \code{dsomop_result} with per-site and optionally pooled data. Pooling
#' sums counts across servers and re-ranks.
#'
#' @param table Character; the CDM table name (e.g.,
#'   \code{"condition_occurrence"}, \code{"drug_exposure"}).
#' @param concept_col Character; the concept column name, or NULL for
#'   automatic detection based on the table's standard concept column
#'   (default: NULL).
#' @param metric Character; \code{"persons"} (default) to rank by distinct
#'   person count, or \code{"records"} to rank by total record count.
#' @param top_n Integer; number of top concepts to return (default: 50).
#' @param cohort_table Character; name of a server-side cohort temp table
#'   to restrict the analysis to a specific cohort (default: NULL).
#' @param window List with \code{start} and \code{end} date strings
#'   (ISO 8601) for temporal filtering, or NULL for no date restriction
#'   (default: NULL).
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) requires all
#'   servers to succeed, \code{"pooled_only_ok"} allows partial results.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of data frames with columns \code{concept_id}, \code{concept_name},
#'   \code{count_value}, etc.), \code{$pooled} (combined data frame when
#'   scope is \code{"pooled"}, otherwise NULL), and \code{$meta}.
#' @examples
#' \dontrun{
#' result <- ds.omop.concept.prevalence("condition_occurrence")
#' head(result$per_site$server1)
#'
#' pooled <- ds.omop.concept.prevalence("drug_exposure",
#'                                       metric = "records",
#'                                       scope = "pooled")
#' pooled$pooled
#' }
#' @export
ds.omop.concept.prevalence <- function(table, concept_col = NULL,
                                        metric = "persons", top_n = 50,
                                        cohort_table = NULL, window = NULL,
                                        scope = c("per_site", "pooled"),
                                        pooling_policy = "strict",
                                        symbol = "omop", conns = NULL,
                                        execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.concept.prevalence",
    table = table, concept_col = concept_col, metric = metric,
    top_n = top_n, cohort_table = cohort_table, window = window,
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
    expr = call("omopConceptPrevalenceDS", session$res_symbol,
                table, concept_col, metric, as.integer(top_n),
                cohort_table, window)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "concept_prevalence", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get a disclosure-safe numeric histogram
#'
#' @description
#' Computes a binned histogram for a numeric column in an OMOP CDM table.
#' When scope is \code{"pooled"}, a two-pass algorithm is used: the first
#' pass collects p05/p95 ranges from each server to compute shared bin
#' edges, and the second pass counts records per bin using those shared
#' edges so that results are directly comparable and summable across
#' servers. Bins with counts below the disclosure threshold are suppressed.
#'
#' @param table Character; the CDM table name (e.g., \code{"measurement"}).
#' @param value_col Character; the numeric column to histogram (e.g.,
#'   \code{"value_as_number"}).
#' @param bins Integer; the number of histogram bins (default: 20).
#' @param cohort_table Character; name of a server-side cohort temp table
#'   for filtering, or NULL (default: NULL).
#' @param window List with \code{start}/\code{end} date strings for
#'   temporal filtering, or NULL (default: NULL).
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of data frames with columns \code{bin_start}, \code{bin_end},
#'   \code{count_value}), \code{$pooled} (combined histogram when pooled),
#'   and \code{$meta}.
#' @examples
#' \dontrun{
#' hist_result <- ds.omop.value.histogram("measurement", "value_as_number",
#'                                         bins = 30, scope = "pooled")
#' hist_result$pooled
#' }
#' @export
ds.omop.value.histogram <- function(table, value_col, bins = 20L,
                                     cohort_table = NULL, window = NULL,
                                     scope = c("per_site", "pooled"),
                                     pooling_policy = "strict",
                                     symbol = "omop", conns = NULL,
                                     execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.value.histogram",
    table = table, value_col = value_col, bins = bins,
    cohort_table = cohort_table, window = window,
    scope = scope, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  pooled <- NULL
  warnings <- character(0)

  if (scope == "pooled") {
    # Two-pass pooling: compute shared bin edges across servers
    # Pass 1: Get p05/p95 ranges from each server
    range_raw <- DSI::datashield.aggregate(
      conns,
      expr = call("omopNumericRangeDS", session$res_symbol,
                  table, value_col, cohort_table, window)
    )

    # Compute shared breaks from global p05/p95
    p05s <- vapply(range_raw, function(s) {
      if (is.list(s) && !is.null(s$p05)) s$p05 else NA_real_
    }, numeric(1))
    p95s <- vapply(range_raw, function(s) {
      if (is.list(s) && !is.null(s$p95)) s$p95 else NA_real_
    }, numeric(1))

    global_p05 <- min(p05s, na.rm = TRUE)
    global_p95 <- max(p95s, na.rm = TRUE)

    if (is.finite(global_p05) && is.finite(global_p95) &&
        global_p05 < global_p95) {
      shared_breaks <- seq(global_p05, global_p95, length.out = as.integer(bins) + 1L)

      # Pass 2: Histogram with shared breaks
      raw <- DSI::datashield.aggregate(
        conns,
        expr = call("omopNumericHistogramDS", session$res_symbol,
                    table, value_col, as.integer(bins),
                    cohort_table, window, shared_breaks)
      )

      pool_out <- .pool_result(raw, "histogram", pooling_policy)
      pooled <- pool_out$result
      warnings <- pool_out$warnings
    } else {
      # Fallback: single-pass (ranges are degenerate)
      raw <- DSI::datashield.aggregate(
        conns,
        expr = call("omopNumericHistogramDS", session$res_symbol,
                    table, value_col, as.integer(bins),
                    cohort_table, window)
      )
      pool_out <- .pool_result(raw, "histogram", pooling_policy)
      pooled <- pool_out$result
      warnings <- c(pool_out$warnings,
                     "Degenerate range: fell back to single-pass histogram")
    }
  } else {
    # Per-site: single pass (no pooling needed)
    raw <- DSI::datashield.aggregate(
      conns,
      expr = call("omopNumericHistogramDS", session$res_symbol,
                  table, value_col, as.integer(bins),
                  cohort_table, window)
    )
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get disclosure-safe numeric quantiles
#'
#' @description
#' Computes quantiles for a numeric column in an OMOP CDM table. Quantile
#' computation happens entirely on the server side to avoid exposing
#' individual-level data. Note that quantiles are inherently non-poolable
#' from summary statistics alone; when \code{scope = "pooled"} is
#' requested, per-site quantiles are still returned but a warning is
#' emitted and the \code{$pooled} slot remains NULL.
#'
#' @param table Character; the CDM table name (e.g., \code{"measurement"}).
#' @param value_col Character; the numeric column name (e.g.,
#'   \code{"value_as_number"}).
#' @param probs Numeric vector; the quantile probabilities to compute
#'   (default: \code{c(0.05, 0.25, 0.5, 0.75, 0.95)}).
#' @param cohort_table Character; name of a server-side cohort temp table
#'   for filtering, or NULL (default: NULL).
#' @param window List with \code{start}/\code{end} date strings for
#'   temporal filtering, or NULL (default: NULL).
#' @param rounding Integer; number of decimal places to round quantile
#'   values to (default: 2).
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#'   Pooled quantiles are not computed (see Description).
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of named numeric vectors or data frames with quantile values),
#'   \code{$pooled} (always NULL since quantiles cannot be safely pooled),
#'   and \code{$meta} (includes warnings when pooled scope is requested).
#' @examples
#' \dontrun{
#' q <- ds.omop.value.quantiles("measurement", "value_as_number",
#'                               probs = c(0.25, 0.5, 0.75))
#' q$per_site$server1
#' }
#' @export
ds.omop.value.quantiles <- function(table, value_col,
                                     probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                     cohort_table = NULL, window = NULL,
                                     rounding = 2L,
                                     scope = c("per_site", "pooled"),
                                     pooling_policy = "strict",
                                     symbol = "omop", conns = NULL,
                                     execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.value.quantiles",
    table = table, value_col = value_col, probs = probs,
    cohort_table = cohort_table, window = window,
    rounding = rounding, scope = scope, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopNumericQuantilesDS", session$res_symbol,
                table, value_col, probs,
                cohort_table, window, as.integer(rounding))
  )

  # Quantiles are NOT safely poolable without individual-level data
  warnings <- character(0)
  if (scope == "pooled") {
    warnings <- "Quantiles cannot be safely pooled without individual-level data"
  }

  dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get record counts by time period
#'
#' @description
#' Aggregates records in an OMOP CDM table by time period (year, quarter,
#' or month) and returns disclosure-safe counts. The date column is
#' auto-detected from the table schema if not specified. This is useful
#' for understanding temporal trends in data coverage and identifying
#' gaps or spikes in data collection.
#'
#' @param table Character; the CDM table name (e.g.,
#'   \code{"condition_occurrence"}, \code{"drug_exposure"}).
#' @param date_col Character; the date column to aggregate by, or NULL
#'   for automatic detection based on the table's standard date column
#'   (default: NULL).
#' @param granularity Character; the time granularity for aggregation:
#'   \code{"year"} (default), \code{"quarter"}, or \code{"month"}.
#' @param cohort_table Character; name of a server-side cohort temp table
#'   for filtering, or NULL (default: NULL).
#' @param window List with \code{start}/\code{end} date strings for
#'   temporal filtering, or NULL (default: NULL).
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of data frames with columns \code{period} and \code{count_value}),
#'   \code{$pooled} (combined counts when pooled), and \code{$meta}.
#' @examples
#' \dontrun{
#' monthly <- ds.omop.date.counts("condition_occurrence",
#'                                 granularity = "month",
#'                                 scope = "pooled")
#' monthly$pooled
#' }
#' @export
ds.omop.date.counts <- function(table, date_col = NULL,
                                 granularity = "year",
                                 cohort_table = NULL, window = NULL,
                                 scope = c("per_site", "pooled"),
                                 pooling_policy = "strict",
                                 symbol = "omop", conns = NULL,
                                 execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.date.counts",
    table = table, date_col = date_col, granularity = granularity,
    cohort_table = cohort_table, window = window,
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
    expr = call("omopDateCountsDS", session$res_symbol,
                table, date_col, granularity,
                cohort_table, window)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "date_counts", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get concept drilldown profile
#'
#' @description
#' Returns a comprehensive drilldown profile for a single concept within
#' an OMOP CDM table. The profile includes summary statistics (record
#' count, person count), numeric distribution (if applicable), categorical
#' value breakdown, date coverage range, and missingness rates for
#' associated columns. All results are disclosure-controlled on the server
#' side before being returned.
#'
#' @param table Character; the CDM table name (e.g.,
#'   \code{"condition_occurrence"}, \code{"measurement"}).
#' @param concept_id Integer; the OMOP concept ID to drill into.
#' @param concept_col Character; the concept column to drill into, or NULL
#'   for automatic detection based on the table's standard concept column
#'   (default: NULL).
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of lists containing \code{summary}, \code{numeric}, \code{categorical},
#'   \code{date_range}, and \code{missingness} components), \code{$pooled}
#'   (combined profile when pooled), and \code{$meta}.
#' @examples
#' \dontrun{
#' profile <- ds.omop.concept.drilldown("condition_occurrence",
#'                                       concept_id = 201820)
#' profile$per_site$server1$summary
#' profile$per_site$server1$numeric
#' }
#' @export
ds.omop.concept.drilldown <- function(table, concept_id,
                                       concept_col = NULL,
                                       scope = c("per_site", "pooled"),
                                       pooling_policy = "strict",
                                       symbol = "omop", conns = NULL,
                                       execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.concept.drilldown",
    table = table, concept_id = concept_id,
    concept_col = concept_col,
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
    expr = if (!is.null(concept_col)) {
      call("omopConceptDrilldownDS", session$res_symbol,
           table, as.integer(concept_id), concept_col)
    } else {
      call("omopConceptDrilldownDS", session$res_symbol,
           table, as.integer(concept_id))
    }
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "concept_drilldown", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Locate concept across all CDM tables
#'
#' @description
#' Searches all OMOP CDM tables that contain concept columns and returns
#' a presence matrix showing which tables contain the specified concept
#' IDs. This is useful for understanding where a concept appears in the
#' database before performing deeper exploration or extraction. Counts
#' are disclosure-controlled; tables where a concept appears fewer than
#' the threshold number of times are reported as suppressed.
#'
#' @param concept_ids Integer vector; one or more OMOP concept IDs to
#'   search for across all CDM tables.
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of data frames with columns \code{concept_id}, \code{table_name},
#'   \code{count_value}), \code{$pooled} (combined presence matrix when
#'   pooled), and \code{$meta}.
#' @examples
#' \dontrun{
#' loc <- ds.omop.concept.locate(c(201820, 316139))
#' loc$per_site$server1
#' }
#' @export
ds.omop.concept.locate <- function(concept_ids,
                                    scope = c("per_site", "pooled"),
                                    pooling_policy = "strict",
                                    symbol = "omop", conns = NULL,
                                    execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.concept.locate",
    concept_ids = concept_ids,
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
    expr = call("omopLocateConceptDS", session$res_symbol,
                as.integer(concept_ids))
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "concept_locate", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}
