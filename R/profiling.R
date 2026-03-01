# Module: Data Profiling
# Client-side wrappers for table statistics, column statistics, domain
# coverage, missingness, and value counts.

#' Get table-level statistics
#'
#' @description
#' Returns row counts and person counts for the specified OMOP CDM table.
#' Results are disclosure-controlled on the server side (counts below the
#' minimum cell threshold are suppressed) and returned per-site with
#' optional pooled totals that sum counts across servers.
#'
#' @param table Character; the CDM table name (e.g.,
#'   \code{"condition_occurrence"}, \code{"person"}).
#' @param stats Character vector; which statistics to compute. Supported
#'   values are \code{"rows"} (total row count) and \code{"persons"}
#'   (distinct person count). Default: \code{c("rows", "persons")}.
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) requires all
#'   servers to succeed, \code{"pooled_only_ok"} allows partial results.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of data frames with columns \code{statistic} and \code{value}),
#'   \code{$pooled} (combined statistics when pooled), and \code{$meta}.
#' @examples
#' \dontrun{
#' result <- ds.omop.table.stats("condition_occurrence")
#' result$per_site$server1
#'
#' pooled <- ds.omop.table.stats("drug_exposure", scope = "pooled")
#' pooled$pooled
#' }
#' @export
ds.omop.table.stats <- function(table,
                                stats = c("rows", "persons"),
                                scope = c("per_site", "pooled"),
                                pooling_policy = "strict",
                                symbol = "omop",
                                conns = NULL,
                                execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.table.stats",
    table = table, stats = stats,
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
    expr = call("omopTableStatsDS", session$res_symbol,
                table, stats)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "table_stats", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get column-level statistics
#'
#' @description
#' Returns detailed statistics for a single column in an OMOP CDM table,
#' including data type, non-null count, distinct count, and (for numeric
#' columns) min, max, mean, and standard deviation. All counts are
#' disclosure-controlled on the server side.
#'
#' @param table Character; the CDM table name (e.g.,
#'   \code{"condition_occurrence"}).
#' @param column Character; the column name to profile (e.g.,
#'   \code{"condition_start_date"}, \code{"value_as_number"}).
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of data frames or lists with column-level statistics), \code{$pooled}
#'   (combined statistics when pooled), and \code{$meta}.
#' @examples
#' \dontrun{
#' col_info <- ds.omop.column.stats("measurement", "value_as_number")
#' col_info$per_site$server1
#' }
#' @export
ds.omop.column.stats <- function(table, column,
                                 scope = c("per_site", "pooled"),
                                 pooling_policy = "strict",
                                 symbol = "omop",
                                 conns = NULL,
                                 execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.column.stats",
    table = table, column = column,
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
    expr = call("omopColumnStatsDS", session$res_symbol,
                table, column)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "column_stats", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get cross-table domain coverage
#'
#' @description
#' Provides a high-level overview of data coverage across all OMOP CDM
#' domain tables. For each clinical domain (conditions, drugs, procedures,
#' measurements, etc.), returns row counts, person counts, and date ranges.
#' This is useful for quickly assessing which data domains are populated
#' and to what extent, without needing to query each table individually.
#'
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of data frames with columns \code{table_name}, \code{row_count},
#'   \code{person_count}), \code{$pooled} (combined coverage when pooled),
#'   and \code{$meta}.
#' @examples
#' \dontrun{
#' coverage <- ds.omop.domain.coverage(scope = "pooled")
#' coverage$pooled
#' }
#' @export
ds.omop.domain.coverage <- function(scope = c("per_site", "pooled"),
                                    pooling_policy = "strict",
                                    symbol = "omop",
                                    conns = NULL,
                                    execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.domain.coverage",
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
    expr = call("omopDomainCoverageDS", session$res_symbol)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "domain_coverage", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get missingness rates for columns
#'
#' @description
#' Computes the proportion and count of NULL/missing values for each
#' column (or a subset of columns) in an OMOP CDM table. This is
#' essential for data quality assessment, helping identify columns with
#' high missingness that may affect downstream analyses. Results are
#' disclosure-controlled; columns where the non-null count falls below
#' the threshold are suppressed.
#'
#' @param table Character; the CDM table name (e.g.,
#'   \code{"measurement"}, \code{"observation"}).
#' @param columns Character vector; specific column names to check, or
#'   NULL to check all columns in the table (default: NULL).
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of data frames with columns \code{column_name}, \code{total_count},
#'   \code{null_count}, \code{missing_pct}), \code{$pooled} (combined
#'   missingness when pooled), and \code{$meta}.
#' @examples
#' \dontrun{
#' miss <- ds.omop.missingness("measurement",
#'                              columns = c("value_as_number",
#'                                          "value_as_concept_id"))
#' miss$per_site$server1
#' }
#' @export
ds.omop.missingness <- function(table, columns = NULL,
                                scope = c("per_site", "pooled"),
                                pooling_policy = "strict",
                                symbol = "omop",
                                conns = NULL,
                                execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.missingness",
    table = table, columns = columns,
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
    expr = call("omopMissingnessDS", session$res_symbol,
                table, columns)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "missingness", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Get value frequencies for a column
#'
#' @description
#' Returns the top-N most frequent distinct values for a column in an
#' OMOP CDM table, along with their counts. This is useful for profiling
#' categorical or low-cardinality columns such as \code{type_concept_id}
#' or \code{unit_concept_id}. Counts below the disclosure threshold are
#' suppressed, and concept names are resolved where available.
#'
#' @param table Character; the CDM table name (e.g.,
#'   \code{"condition_occurrence"}).
#' @param column Character; the column to count distinct values for
#'   (e.g., \code{"condition_type_concept_id"}).
#' @param top_n Integer; the number of most frequent values to return
#'   (default: 20).
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list
#'   of data frames with columns \code{value}, \code{count_value}),
#'   \code{$pooled} (combined value counts when pooled), and \code{$meta}.
#' @examples
#' \dontrun{
#' vc <- ds.omop.value.counts("condition_occurrence",
#'                             "condition_type_concept_id",
#'                             top_n = 10, scope = "pooled")
#' vc$pooled
#' }
#' @export
ds.omop.value.counts <- function(table, column, top_n = 20,
                                 scope = c("per_site", "pooled"),
                                 pooling_policy = "strict",
                                 symbol = "omop",
                                 conns = NULL,
                                 execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.value.counts",
    table = table, column = column, top_n = top_n,
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
    expr = call("omopValueCountsDS", session$res_symbol,
                table, column, as.integer(top_n), TRUE)
  )

  pooled <- NULL
  warnings <- character(0)
  if (scope == "pooled") {
    pool_out <- .pool_result(raw, "value_counts", pooling_policy)
    pooled <- pool_out$result
    warnings <- pool_out$warnings
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}
