# Module: Data Exploration
# Client-side wrappers for OMOP CDM data profiling and exploration functions.

#' Get safe numeric cutpoints for a column
#'
#' @description
#' Returns a public numeric grid configured by the data controller for an OMOP
#' column. The server releases the complete grid only when every bin is
#' supported by the minimum number of distinct persons after one-contribution-
#' per-person reduction. Edges are not estimated from protected values.
#'
#' @param table Character; the OMOP CDM table name (e.g.,
#'   \code{"measurement"}, \code{"observation"}).
#' @param column Character; the numeric column to bin (e.g.,
#'   \code{"value_as_number"}).
#' @param concept_id Integer or NULL; optional concept ID to restrict
#'   rows before computing bins (default: NULL for all rows).
#' @param n_bins Integer; exact number of bins in the controller-configured
#'   public grid (default: 10). Unsupported or under-populated grids fail
#'   closed; bins are never merged based on protected counts.
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#'   Cutpoints are inherently per-site; pooled scope is accepted but the
#'   pooled slot will be NULL.
#' @param symbol Character; the session symbol identifying the OMOP
#'   connection (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. When omitted, legacy \code{scope} behaviour is preserved.
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   where each element contains public \code{breaks}, banded \code{counts}, a
#'   session \code{contract}, and clipping/grid metadata),
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
                                    execute = TRUE, type = NULL) {
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing
  )

  code <- .build_code("ds.omop.safe.cutpoints",
    table = table, column = column, concept_id = concept_id,
    n_bins = n_bins, scope = if (is.null(type)) scope else NULL,
    type = if (is.null(type)) NULL else result_type, symbol = symbol)

  if (!execute) {
    return(.result_type_view(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)), result_type))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopSafeCutpointsDS", session$res_symbol,
                table, column, concept_id, as.integer(n_bins))
  )

  ds_errors <- attr(raw, "ds_errors")
  warnings <- if (!is.null(ds_errors)) {
    paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  } else character(0)

  .result_type_view(dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = scope, warnings = warnings)),
    result_type,
    "Safe cutpoints are site-specific and cannot be combined.")
}

#' Resolve one numeric-bin contract that every connected site issued
#'
#' @keywords internal
.common_safe_bins <- function(cuts, table, column) {
  if (!inherits(cuts, "dsomop_result")) {
    stop("cuts must be a dsomop_result from ds.omop.safe.cutpoints().",
         call. = FALSE)
  }
  if (length(cuts$meta$warnings %||% character(0)) > 0L) {
    stop("At least one connected site did not issue safe numeric cutpoints; ",
         "a shared recipe was not created.", call. = FALSE)
  }
  site_bins <- cuts$per_site
  valid <- vapply(site_bins, function(x) {
    is.list(x) && is.numeric(x$breaks) && length(x$breaks) >= 3L &&
      all(is.finite(x$breaks)) && is.list(x$contract)
  }, logical(1))
  if (length(site_bins) == 0L || !all(valid)) {
    stop("Every connected site must return one complete safe numeric grid for ",
         table, ".", column, ".", call. = FALSE)
  }

  contracts <- lapply(site_bins, `[[`, "contract")
  contract_json <- vapply(contracts, jsonlite::toJSON, character(1),
                          auto_unbox = TRUE, null = "null")
  if (length(unique(contract_json)) != 1L) {
    stop("Connected sites returned incompatible numeric-bin scopes.",
         call. = FALSE)
  }
  breaks <- Reduce(intersect,
                   lapply(site_bins, function(x) as.numeric(x$breaks)))
  breaks <- sort(unique(breaks[is.finite(breaks)]))
  if (length(breaks) < 2L) {
    stop("Connected sites have no common server-issued numeric interval; ",
         "run site-specific recipes or configure a shared public grid.",
         call. = FALSE)
  }
  list(breaks = breaks, contract = contracts[[1]])
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
  safe_bins <- .common_safe_bins(cuts, table, column)
  omop_filter_value(column = column, threshold = threshold,
                     direction = direction,
                     safe_bins = safe_bins)
}

#' Create a safe population filter for a numeric measurement interval
#'
#' Requests the controller-configured public grid for one measurement concept,
#' then snaps the requested closed-open interval outwards to edges issued by
#' every connected site. The result is executable as a population-level
#' \code{has_measurement} filter. Exact or one-sided client-authored thresholds
#' are deliberately not supported.
#'
#' @param concept_id One measurement concept ID.
#' @param min_value,max_value Finite requested interval limits. Both are
#'   required and must lie inside the common public grid.
#' @inheritParams ds.omop.safe.filter.value
#' @return An authenticated population-level \code{omop_filter}.
#' @export
ds.omop.safe.filter.measurement <- function(concept_id, min_value, max_value,
                                             n_bins = 10L,
                                             symbol = "omop", conns = NULL) {
  concept_id <- suppressWarnings(as.integer(concept_id))
  min_value <- suppressWarnings(as.numeric(min_value))
  max_value <- suppressWarnings(as.numeric(max_value))
  if (length(concept_id) != 1L || is.na(concept_id) ||
      length(min_value) != 1L || length(max_value) != 1L ||
      !is.finite(min_value) || !is.finite(max_value) ||
      min_value >= max_value) {
    stop("concept_id must be one integer and min_value/max_value must be a ",
         "finite increasing interval.", call. = FALSE)
  }
  cuts <- ds.omop.safe.cutpoints(
    "measurement", "value_as_number", concept_id = concept_id,
    n_bins = n_bins, scope = "per_site", symbol = symbol, conns = conns
  )
  safe_bins <- .common_safe_bins(cuts, "measurement", "value_as_number")
  edges <- safe_bins$breaks
  if (min_value < min(edges) || max_value > max(edges)) {
    stop("Requested measurement interval lies outside the common public grid.",
         call. = FALSE)
  }
  lower <- max(edges[edges <= min_value])
  upper <- min(edges[edges >= max_value])
  omop_filter_has_measurement(
    concept_id = concept_id,
    min_value = lower,
    max_value = upper,
    safe_bins = safe_bins
  )
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
#' @param offset Integer; number of ranked concepts to skip for pagination
#'   (default: 0).
#' @param global Logical; if \code{TRUE}, rank concepts across all supported
#'   clinical tables rather than only \code{table} (default: \code{FALSE}).
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side cohort table name), or NULL.
#'   Takes precedence over \code{cohort_table}.
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) requires all
#'   servers to succeed, \code{"pooled_only_ok"} allows partial results.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. When omitted, legacy \code{scope} behaviour is preserved.
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
ds.omop.concept.prevalence <- function(table = NULL, concept_col = NULL,
                                        metric = "persons", top_n = 50,
                                        cohort_table = NULL, window = NULL,
                                        offset = 0L, global = FALSE,
                                        cohort = NULL,
                                        scope = c("per_site", "pooled"),
                                        pooling_policy = "strict",
                                        symbol = "omop", conns = NULL,
                                        execute = TRUE, type = NULL) {
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing
  )
  cohort_scope <- .cohort_scope_arg(cohort) %||% cohort_table

  code <- .build_code("ds.omop.concept.prevalence",
    table = table, concept_col = concept_col, metric = metric,
    top_n = top_n, cohort_table = cohort_scope, window = window,
    offset = offset, global = global,
    scope = if (is.null(type)) scope else NULL,
    type = if (is.null(type)) NULL else result_type, symbol = symbol)

  if (!execute) {
    return(.result_type_view(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)), result_type))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopConceptPrevalenceDS", session$res_symbol,
                table, concept_col, metric, as.integer(top_n),
                cohort_scope, window, offset = as.integer(offset),
                global = isTRUE(global))
  )

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (.result_type_wants_combine(result_type) && length(raw) > 0) {
    pool_out <- .pool_result(raw, "concept_prevalence", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
  }

  .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings)),
    result_type,
    "No complete disclosure-safe pooled concept prevalence was available.")
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
#' @param concept_id Integer or NULL; optional concept ID to restrict rows to a
#'   single concept of the table before binning (e.g. \code{value_as_number}
#'   for one measurement concept). Default: NULL for all rows. The server
#'   applies the same disclosure controls to the concept-filtered population.
#'   Requires a dsOMOP server build with histogram concept scoping; older
#'   servers reject the argument (use the concept-scoped quantiles aggregate
#'   via \code{ds.omop.value.quantiles()} as a fallback).
#' @param cohort_table Character; name of a server-side cohort temp table
#'   for filtering, or NULL (default: NULL).
#' @param window List with \code{start}/\code{end} date strings for
#'   temporal filtering, or NULL (default: NULL).
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side cohort table name), or NULL.
#'   Takes precedence over \code{cohort_table}.
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @param plot Logical; if \code{TRUE}, draw a federation-wide bar chart of the
#'   pooled, shared-edge bins (forces \code{scope = "pooled"}) and return the
#'   result invisibly. Default \code{FALSE}.
#' @param nbins Integer; number of display bins used when \code{plot = TRUE}
#'   (default: 9).
#' @param xlab,main,col Axis label, title and bar colour used when
#'   \code{plot = TRUE}.
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. A pooled plot requires \code{"combine"} or \code{"both"}.
#'   When omitted, legacy \code{scope} behaviour is preserved.
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
                                     concept_id = NULL,
                                     cohort_table = NULL, window = NULL,
                                     cohort = NULL,
                                     scope = c("per_site", "pooled"),
                                     pooling_policy = "strict",
                                     symbol = "omop", conns = NULL,
                                     execute = TRUE,
                                     plot = FALSE, nbins = 9L, xlab = NULL,
                                     main = NULL, col = "#4C72B0",
                                     type = NULL) {
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  pooling_policy <- match.arg(pooling_policy,
                              c("strict", "pooled_only_ok"))
  # A federation-wide plot must pass through the same pooling policy as the
  # returned data; it must never sum per-site frames behind a failed pool.
  if (isTRUE(plot)) {
    if (!is.null(type) && identical(.normalize_result_type(type), "split")) {
      stop("plot = TRUE requires type = 'combine' or type = 'both'.",
           call. = FALSE)
    }
    scope <- "pooled"
    if (is.null(type)) scope_missing <- FALSE
  }
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing
  )
  cohort_scope <- .cohort_scope_arg(cohort) %||% cohort_table

  code <- .build_code("ds.omop.value.histogram",
    table = table, value_col = value_col, bins = bins,
    concept_id = concept_id, cohort_table = cohort_scope, window = window,
    scope = if (is.null(type)) scope else NULL,
    type = if (is.null(type)) NULL else result_type, symbol = symbol)

  if (!execute) {
    return(.result_type_view(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)), result_type))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  pooled <- NULL
  warnings <- character(0)

  # Only pass concept_id when set, so the unfiltered path stays byte-for-byte
  # compatible with servers that predate histogram concept scoping (they would
  # reject an extra concept_id = NULL argument). When a concept IS requested
  # the call requires a server build with concept scoping.
  .hist_call <- function(...) {
    args <- list("omopNumericHistogramDS", session$res_symbol,
                 table, value_col, as.integer(bins), cohort_scope, window, ...)
    if (!is.null(concept_id)) args$concept_id <- concept_id
    do.call(call, args)
  }
  .range_call <- function() {
    args <- list("omopNumericRangeDS", session$res_symbol,
                 table, value_col, cohort_scope, window)
    if (!is.null(concept_id)) args$concept_id <- concept_id
    do.call(call, args)
  }

  if (.result_type_wants_combine(result_type)) {
    # Two-pass pooling: compute shared bin edges across servers
    # Pass 1: Get p05/p95 ranges from each server
    range_raw <- .ds_safe_aggregate(conns, expr = .range_call())
    range_errors <- attr(range_raw, "ds_errors") %||% list()
    missing_ranges <- setdiff(names(conns), names(range_raw))
    for (server in missing_ranges) {
      if (is.null(range_errors[[server]])) {
        range_errors[[server]] <- "server returned no verifiable range result"
      }
    }
    usable_range <- vapply(range_raw, function(value) {
      if (!is.list(value) || is.null(value$p05) || is.null(value$p95)) {
        return(FALSE)
      }
      p05 <- suppressWarnings(as.numeric(value$p05))
      p95 <- suppressWarnings(as.numeric(value$p95))
      length(p05) == 1L && length(p95) == 1L &&
        is.finite(p05) && is.finite(p95) && p05 <= p95
    }, logical(1L))
    invalid_ranges <- names(range_raw)[!usable_range]
    for (server in invalid_ranges) {
      range_errors[[server]] <- "server returned no usable disclosure-safe range"
    }
    eligible_servers <- names(range_raw)[usable_range]

    if (pooling_policy == "strict" && length(range_errors) > 0L) {
      # No histogram query was issued, so do not expose range objects in the
      # per-site histogram slot. Preserve the failed range nodes as errors.
      raw <- list()
      attr(raw, "ds_errors") <- range_errors
      warnings <- paste0(
        "Strict pooling failed before histogram calculation: incomplete ",
        "range federation; unavailable server(s): ",
        paste(names(range_errors), collapse = ", "), "."
      )
    } else if (length(eligible_servers) == 0L) {
      raw <- list()
      if (length(range_errors) > 0L) attr(raw, "ds_errors") <- range_errors
      warnings <- "No server returned a usable disclosure-safe histogram range."
    } else {
      # The second pass must use exactly the nodes represented in the shared
      # range. Otherwise a recovered node could be counted against edges it did
      # not help define, silently truncating its distribution.
      histogram_conns <- conns[eligible_servers]
      p05s <- vapply(range_raw[eligible_servers], function(s) {
        as.numeric(s$p05)
      }, numeric(1L))
      p95s <- vapply(range_raw[eligible_servers], function(s) {
        as.numeric(s$p95)
      }, numeric(1L))
      global_p05 <- min(p05s)
      global_p95 <- max(p95s)

      if (global_p05 < global_p95) {
        shared_breaks <- seq(
          global_p05, global_p95, length.out = as.integer(bins) + 1L
        )
        raw <- .ds_safe_aggregate(
          histogram_conns,
          expr = .hist_call(.ds_encode(shared_breaks))
        )
      } else {
        raw <- .ds_safe_aggregate(histogram_conns, expr = .hist_call())
        warnings <- c(
          warnings,
          "Degenerate range: fell back to single-pass histogram"
        )
      }

      histogram_errors <- attr(raw, "ds_errors") %||% list()
      all_errors <- c(range_errors, histogram_errors)
      if (length(all_errors) > 0L) attr(raw, "ds_errors") <- all_errors
      if (pooling_policy == "pooled_only_ok" && length(range_errors) > 0L) {
        warnings <- c(
          warnings,
          paste0(
            "Pooled only servers with a usable shared-range contribution: ",
            paste(eligible_servers, collapse = ", "), ". Excluded: ",
            paste(names(range_errors), collapse = ", "), "."
          )
        )
      }
      pool_out <- .pool_result(raw, "histogram", pooling_policy)
      pooled <- pool_out$result
      warnings <- c(warnings, pool_out$warnings)
    }
  } else {
    # Per-site: single pass (no pooling needed)
    raw <- .ds_safe_aggregate(conns, expr = .hist_call())
  }

  out <- .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings)),
    result_type,
    "No complete disclosure-safe pooled histogram was available.")

  if (isTRUE(plot)) {
    .omopPlotHistogram(out, nbins = nbins, xlab = xlab %||% value_col,
                       main = main, col = col)
    return(invisible(out))
  }
  out
}

#' Draw a federation-wide histogram from a value-histogram result
#'
#' Sums the disclosure-safe, shared-edge bins across sites into one bar chart.
#' Used by \code{ds.omop.value.histogram(plot = TRUE)} so callers get a plot
#' directly instead of hand-combining per-site bins.
#' @keywords internal
.omopPlotHistogram <- function(hist_result, nbins = 9L, xlab = NULL,
                                main = NULL, col = "#4C72B0") {
  # Re-bin only the disclosure-controlled pooled histogram. Falling back to a
  # sum of per-site frames here would bypass strict federation failure.
  df <- hist_result$pooled
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 ||
      !all(c("bin_start", "bin_end") %in% names(df))) {
    warning("No pooled histogram data to plot (pooling failed or all bins are suppressed/empty).")
    return(invisible(NULL))
  }
  cc <- intersect(c("count", "count_value", "n"), names(df))[1]
  if (is.na(cc)) { warning("No count column in histogram result."); return(invisible(NULL)) }
  df <- df[!is.na(df[[cc]]), , drop = FALSE]
  mid <- (df$bin_start + df$bin_end) / 2
  br  <- seq(min(mid), max(mid), length.out = as.integer(nbins) + 1L)
  grp <- cut(mid, breaks = br, include.lowest = TRUE)
  counts <- tapply(df[[cc]], grp, sum); counts[is.na(counts)] <- 0
  centres <- (utils::head(br, -1) + utils::tail(br, -1)) / 2
  # Adaptive label precision (whole numbers for wide ranges like years, one
  # decimal for narrow ranges like creatinine) keeps the vertical tick labels
  # short, and an expanded bottom margin places the x-axis title *below* them
  # rather than overlapping (the original bug, worst for 4-digit years).
  digits <- if (diff(range(centres)) >= 20) 0L else 1L
  labs   <- formatC(centres, format = "f", digits = digits)
  op  <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op))
  bot <- 4 + 0.55 * max(nchar(labs))
  graphics::par(mar = c(bot, 4.1, 3.1, 1.1))
  graphics::barplot(as.numeric(counts), names.arg = labs, las = 2,
                    col = col, border = NA, ylab = "count", xlab = "",
                    main = main %||% "Distribution (pooled across sites)")
  graphics::title(xlab = xlab %||% "value", line = bot - 1.3)
  invisible(data.frame(centre = centres, count = as.numeric(counts)))
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
#' @param concept_id Integer or NULL; optional concept ID to restrict
#'   rows to a single concept of the table before computing quantiles
#'   (e.g., \code{value_as_number} for one measurement concept).
#'   Default: NULL for all rows. The server applies the same disclosure
#'   controls (including the [0.05, 0.95] probability clamp that blocks
#'   min/max) to the concept-filtered population.
#' @param cohort_table Character; name of a server-side cohort temp table
#'   for filtering, or NULL (default: NULL).
#' @param window List with \code{start}/\code{end} date strings for
#'   temporal filtering, or NULL (default: NULL).
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side cohort table name), or NULL.
#'   Takes precedence over \code{cohort_table}.
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
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. Quantiles are not poolable; \code{"combine"} therefore
#'   returns no pooled value and records the reason. When omitted, legacy
#'   \code{scope} behaviour is preserved.
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
                                     concept_id = NULL,
                                     cohort_table = NULL, window = NULL,
                                     cohort = NULL,
                                     rounding = 2L,
                                     scope = c("per_site", "pooled"),
                                     pooling_policy = "strict",
                                     symbol = "omop", conns = NULL,
                                     execute = TRUE, type = NULL) {
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing
  )
  cohort_scope <- .cohort_scope_arg(cohort) %||% cohort_table

  code <- .build_code("ds.omop.value.quantiles",
    table = table, value_col = value_col, probs = probs,
    concept_id = concept_id, cohort_table = cohort_scope, window = window,
    rounding = rounding, scope = if (is.null(type)) scope else NULL,
    type = if (is.null(type)) NULL else result_type, symbol = symbol)

  if (!execute) {
    return(.result_type_view(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)), result_type))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopNumericQuantilesDS", session$res_symbol,
                table, value_col, .ds_encode(probs),
                cohort_scope, window, as.integer(rounding),
                concept_id = concept_id)
  )

  ds_errors <- attr(raw, "ds_errors")
  # Quantiles are NOT safely poolable without individual-level data
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (.result_type_wants_combine(result_type)) {
    warnings <- c(warnings,
      "Quantiles cannot be safely pooled without individual-level data")
  }

  .result_type_view(dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings)),
    result_type,
    "Quantiles cannot be safely pooled without individual-level data")
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
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. When omitted, legacy \code{scope} behaviour is preserved.
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
                                 execute = TRUE, type = NULL) {
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing
  )

  code <- .build_code("ds.omop.date.counts",
    table = table, date_col = date_col, granularity = granularity,
    cohort_table = cohort_table, window = window,
    scope = if (is.null(type)) scope else NULL,
    type = if (is.null(type)) NULL else result_type, symbol = symbol)

  if (!execute) {
    return(.result_type_view(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)), result_type))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopDateCountsDS", session$res_symbol,
                table, date_col, granularity,
                cohort_table, window)
  )

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (.result_type_wants_combine(result_type) && length(raw) > 0) {
    pool_out <- .pool_result(raw, "date_counts", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
  }

  .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings)),
    result_type,
    "No complete disclosure-safe pooled date counts were available.")
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
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. When omitted, legacy \code{scope} behaviour is preserved.
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
                                       execute = TRUE, type = NULL) {
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing
  )

  code <- .build_code("ds.omop.concept.drilldown",
    table = table, concept_id = concept_id,
    concept_col = concept_col,
    scope = if (is.null(type)) scope else NULL,
    type = if (is.null(type)) NULL else result_type, symbol = symbol)

  if (!execute) {
    return(.result_type_view(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)), result_type))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  the_expr <- if (!is.null(concept_col)) {
    call("omopConceptDrilldownDS", session$res_symbol,
         table, as.integer(concept_id), concept_col)
  } else {
    call("omopConceptDrilldownDS", session$res_symbol,
         table, as.integer(concept_id))
  }
  raw <- .ds_safe_aggregate(conns, expr = the_expr)

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (.result_type_wants_combine(result_type) && length(raw) > 0) {
    pool_out <- .pool_result(raw, "concept_drilldown", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
  }

  .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings)),
    result_type,
    "No complete disclosure-safe pooled concept drilldown was available.")
}

#' Summarise a value column scoped to one concept of one table
#'
#' @description
#' Type-aware orchestrator that summarises a value column for a single
#' concept of a single OMOP CDM table. The unit of analysis is the
#' \code{(table, concept_id, column)} triple: in OMOP a value column only
#' makes sense within a concept (a \code{measurement} table mixes HbA1c,
#' weight, blood pressure, ...), so restricting to one concept yields an
#' interpretable distribution. Numeric and categorical value columns
#' receive different DataSHIELD-safe statistics:
#' \itemize{
#'   \item A \code{*_concept_id} value column (e.g.
#'     \code{value_as_concept_id}) is treated as CATEGORICAL and summarised
#'     with \code{\link{ds.omop.value.counts}} (disclosure-safe frequency
#'     counts of the categories observed for this concept).
#'   \item \code{value_as_number} (or any numeric value column) is treated
#'     as NUMERIC and summarised with both \code{\link{ds.omop.column.stats}}
#'     (n, mean, SD, missingness, distinct count) and
#'     \code{\link{ds.omop.value.quantiles}} (median, IQR, percentiles).
#'     Min/max are never returned: the server clamps quantile probabilities
#'     to [0.05, 0.95].
#' }
#' This function only adds a \code{concept_id} filter to queries that are
#' already disclosure-gated server-side; the existing gates fire on the
#' concept-filtered population, so a concept with too few persons is
#' blocked.
#'
#' @param table Character; the CDM table name (e.g., \code{"measurement"},
#'   \code{"observation"}).
#' @param concept_id Integer; the OMOP concept ID to scope the value
#'   column(s) to (e.g., the HbA1c measurement concept).
#' @param column Character or NULL; a single value column to summarise. If
#'   NULL (default), the table's columns are fetched via
#'   \code{\link{ds.omop.columns}} and whichever of
#'   \code{c("value_as_number", "value_as_concept_id")} are present are
#'   summarised.
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#'   Passed through to the underlying calls.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"} (Best Effort). Passed through to the underlying
#'   value-counts, column-stats and quantile calls so that, when pooled,
#'   categories/values present on only some sites are summed across the
#'   available sites rather than suppressed.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param type Optional result view propagated to every aggregate component:
#'   \code{"split"}, \code{"combine"}, or \code{"both"}. When omitted,
#'   legacy \code{scope} behaviour is preserved.
#' @return A named list with \code{table}, \code{concept_id},
#'   \code{numeric} (named-by-column list where each element is a list with
#'   \code{stats} and \code{quantiles} \code{dsomop_result} objects, or
#'   NULL when no numeric value column applies), and \code{categorical}
#'   (named-by-column list of \code{dsomop_result} objects from value
#'   counts, or NULL when no categorical value column applies).
#' @examples
#' \dontrun{
#' # Distribution of value_as_number for an HbA1c measurement concept
#' summ <- ds.omop.concept.summary("measurement", concept_id = 3004410)
#' summ$numeric$value_as_number$stats$per_site$server1
#' summ$numeric$value_as_number$quantiles$per_site$server1
#'
#' # Categorical value_as_concept_id breakdown for an observation concept
#' obs <- ds.omop.concept.summary("observation", concept_id = 4058243,
#'                                 column = "value_as_concept_id")
#' obs$categorical$value_as_concept_id$per_site$server1
#' }
#' @export
ds.omop.concept.summary <- function(table, concept_id, column = NULL,
                                    scope = c("per_site", "pooled"),
                                    pooling_policy = "strict",
                                    symbol = "omop", conns = NULL,
                                    type = NULL) {
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing
  )

  if (!is.null(column)) {
    cols <- column
  } else {
    col_info <- ds.omop.columns(table, symbol = symbol, conns = conns)
    available <- unique(unlist(lapply(col_info, function(df) {
      if (is.data.frame(df) && "column_name" %in% names(df)) {
        as.character(df$column_name)
      } else {
        character(0)
      }
    })))
    cols <- intersect(c("value_as_number", "value_as_concept_id"), available)
  }

  numeric <- list()
  categorical <- list()

  for (col in cols) {
    if (grepl("_concept_id$", col)) {
      categorical[[col]] <- ds.omop.value.counts(
        table, col, concept_id = concept_id,
        type = result_type, pooling_policy = pooling_policy,
        symbol = symbol, conns = conns)
    } else {
      numeric[[col]] <- list(
        stats = ds.omop.column.stats(
          table, col, concept_id = concept_id,
          type = result_type, pooling_policy = pooling_policy,
          symbol = symbol, conns = conns),
        quantiles = ds.omop.value.quantiles(
          table, col, concept_id = concept_id,
          type = result_type, pooling_policy = pooling_policy,
          symbol = symbol, conns = conns))
    }
  }

  list(
    table = table,
    concept_id = concept_id,
    numeric = if (length(numeric) > 0) numeric else NULL,
    categorical = if (length(categorical) > 0) categorical else NULL)
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
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. When omitted, legacy \code{scope} behaviour is preserved.
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
                                    execute = TRUE, type = NULL) {
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing
  )

  code <- .build_code("ds.omop.concept.locate",
    concept_ids = concept_ids,
    scope = if (is.null(type)) scope else NULL,
    type = if (is.null(type)) NULL else result_type, symbol = symbol)

  if (!execute) {
    return(.result_type_view(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)), result_type))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopLocateConceptDS", session$res_symbol,
                .ds_encode(as.integer(concept_ids)))
  )

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (.result_type_wants_combine(result_type) && length(raw) > 0) {
    pool_out <- .pool_result(raw, "concept_locate", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
  }

  .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings)),
    result_type,
    "No complete disclosure-safe pooled concept-location result was available.")
}
