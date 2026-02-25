# ==============================================================================
# dsOMOPClient v2 - Exploration Wrappers (OMOP Studio)
# ==============================================================================

#' Get safe numeric cutpoints for a column
#'
#' Returns bin edges that are safe to use as filter thresholds.
#' Each bin is guaranteed to contain enough persons to pass disclosure.
#'
#' @param table Character; OMOP table name
#' @param column Character; numeric column name
#' @param concept_id Integer or NULL; concept filter
#' @param n_bins Integer; target number of bins (default 10)
#' @param scope Character; "per_site" or "pooled"
#' @param symbol Character; session symbol
#' @param conns DSI connections or NULL
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return dsomop_result with bin edges per server
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

#' Get concept prevalence for a table
#'
#' Returns the top concepts in a table ranked by person count or record count.
#'
#' @param table Character; table name
#' @param concept_col Character; concept column (NULL = auto-detect)
#' @param metric Character; "persons" or "records"
#' @param top_n Integer; number of top concepts
#' @param cohort_table Character; cohort temp table (NULL)
#' @param window List with start/end dates (NULL)
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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

#' Get a safe numeric histogram
#'
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param bins Integer; number of bins
#' @param cohort_table Character; cohort temp table (NULL)
#' @param window List with start/end dates (NULL)
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopNumericHistogramDS", session$res_symbol,
                table, value_col, as.integer(bins),
                cohort_table, window)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "histogram", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get safe numeric quantiles
#'
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param probs Numeric vector; quantile probabilities
#' @param cohort_table Character; cohort temp table (NULL)
#' @param window List with start/end dates (NULL)
#' @param rounding Integer; decimal places
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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

#' Get date counts by time period
#'
#' @param table Character; table name
#' @param date_col Character; date column (NULL = auto-detect)
#' @param granularity Character; "year", "quarter", or "month"
#' @param cohort_table Character; cohort temp table (NULL)
#' @param window List with start/end dates (NULL)
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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
#' Returns a full drilldown profile for a single concept within a table,
#' including summary stats, numeric distribution, categorical values,
#' date coverage, and missingness.
#'
#' @param table Character; table name
#' @param concept_id Integer; concept ID to drill into
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
#' @export
ds.omop.concept.drilldown <- function(table, concept_id,
                                       scope = c("per_site", "pooled"),
                                       pooling_policy = "strict",
                                       symbol = "omop", conns = NULL,
                                       execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.concept.drilldown",
    table = table, concept_id = concept_id,
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
    expr = call("omopConceptDrilldownDS", session$res_symbol,
                table, as.integer(concept_id))
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
#' Searches all CDM tables with concept columns and returns a presence
#' matrix showing where given concept IDs appear.
#'
#' @param concept_ids Integer vector; concept IDs to locate
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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
