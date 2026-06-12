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

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopTableStatsDS", session$res_symbol,
                table, .ds_encode(stats))
  )

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (scope == "pooled" && length(raw) > 0) {
    pool_out <- .pool_result(raw, "table_stats", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
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
#' @param concept_id Integer or NULL; optional concept ID to restrict
#'   rows to a single concept of the table before computing the column
#'   statistics (e.g., \code{value_as_number} for one measurement
#'   concept). Default: NULL for all rows. The server applies the same
#'   disclosure controls to the concept-filtered population.
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
ds.omop.column.stats <- function(table, column, concept_id = NULL,
                                 scope = c("per_site", "pooled"),
                                 pooling_policy = "strict",
                                 symbol = "omop",
                                 conns = NULL,
                                 execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.column.stats",
    table = table, column = column, concept_id = concept_id,
    scope = scope, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopColumnStatsDS", session$res_symbol,
                table, column, concept_id = concept_id)
  )

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (scope == "pooled" && length(raw) > 0) {
    pool_out <- .pool_result(raw, "column_stats", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
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

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopDomainCoverageDS", session$res_symbol)
  )

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (scope == "pooled" && length(raw) > 0) {
    pool_out <- .pool_result(raw, "domain_coverage", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
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

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopMissingnessDS", session$res_symbol,
                table, .ds_encode(columns))
  )

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (scope == "pooled" && length(raw) > 0) {
    pool_out <- .pool_result(raw, "missingness", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
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
#' @param concept_id Integer or NULL; optional concept ID to restrict
#'   rows to a single concept of the table before counting values (e.g.,
#'   the \code{value_as_concept_id} categories for one measurement
#'   concept). Default: NULL for all rows. The server applies the same
#'   disclosure controls to the concept-filtered population.
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
                                 concept_id = NULL,
                                 scope = c("per_site", "pooled"),
                                 pooling_policy = "strict",
                                 symbol = "omop",
                                 conns = NULL,
                                 execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.value.counts",
    table = table, column = column, top_n = top_n,
    concept_id = concept_id, scope = scope, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopValueCountsDS", session$res_symbol,
                table, column, as.integer(top_n),
                concept_id = concept_id)
  )

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (scope == "pooled" && length(raw) > 0) {
    pool_out <- .pool_result(raw, "value_counts", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Disclosure-safe two-way cross-tabulation
#'
#' @description
#' Returns a small-cell-suppressed contingency table cross-tabulating two
#' categorical columns of an OMOP CDM table. The server applies primary
#' small-count suppression PLUS iterative complementary (secondary)
#' suppression to a fixpoint and never returns exact margins, so suppressed
#' cells cannot be recovered by row/column subtraction. Cells below the
#' disclosure threshold render as \code{NA}; structural zeros render as
#' \code{0}. Concept-id axes are decorated with concept names.
#'
#' When \code{stratify_by} is supplied, the result is a named list of
#' independent protected 2-way slices (one per stratum level) rather than a
#' single table; the unstratified total is never returned.
#'
#' @section Cross-tab is descriptive, not inferential:
#' A cross-tab answers "how do these two variables co-occur?". It is NOT a
#' substitute for a multivariable model. For genuine multivariable questions
#' (three or more interacting variables, continuous adjustment, or estimating
#' an association while controlling for confounders), route to
#' \code{\link[dsBaseClient]{ds.glm}} instead of building higher-dimensional
#' cell tables, which are disclosure-unsafe and lose utility on small data.
#'
#' @param table Character; the CDM table name (e.g., \code{"person"}).
#' @param row Character; the column for table rows (e.g.,
#'   \code{"gender_concept_id"}).
#' @param col Character; the column for table columns (e.g.,
#'   \code{"race_concept_id"}).
#' @param by Character; \code{"persons"} (default, distinct person counts) or
#'   \code{"records"} (row counts).
#' @param row_concept_id Integer/vector or NULL; optional concept ID(s) to
#'   restrict the row axis. Default: NULL.
#' @param col_concept_id Integer/vector or NULL; optional concept ID(s) to
#'   restrict the column axis. Default: NULL.
#' @param cohort_table Character or NULL; optional cohort table to scope the
#'   population (inner join on \code{subject_id}). Default: NULL.
#' @param stratify_by Character or NULL; optional third categorical column to
#'   produce a named list of independent stratified 2-way slices. Default:
#'   NULL.
#' @param scope Character; \code{"per_site"} (default) or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}. Under \code{strict}, a cell absent or suppressed
#'   on any site is suppressed in the pooled table.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @return A \code{dsomop_result} object with \code{$per_site} (named list of
#'   server cross-tab objects), \code{$pooled} (cell-wise summed table when
#'   pooled), and \code{$meta}.
#' @examples
#' \dontrun{
#' ct <- ds.omop.crosstab("person", "gender_concept_id", "race_concept_id",
#'                        scope = "pooled")
#' ct$pooled$counts
#' }
#' @export
ds.omop.crosstab <- function(table, row, col, by = "persons",
                             row_concept_id = NULL, col_concept_id = NULL,
                             cohort_table = NULL, stratify_by = NULL,
                             scope = c("per_site", "pooled"),
                             pooling_policy = "strict",
                             symbol = "omop",
                             conns = NULL,
                             execute = TRUE) {
  scope <- match.arg(scope)

  code <- .build_code("ds.omop.crosstab",
    table = table, row = row, col = col, by = by,
    row_concept_id = row_concept_id, col_concept_id = col_concept_id,
    cohort_table = cohort_table, stratify_by = stratify_by,
    scope = scope, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = scope)))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopCrossTabDS", session$res_symbol,
                table, row, col,
                count_mode = by,
                row_concept_ids = row_concept_id,
                col_concept_ids = col_concept_id,
                cohort_table = cohort_table,
                stratify_by = stratify_by)
  )

  ds_errors <- attr(raw, "ds_errors")
  pooled <- NULL
  warnings <- character(0)
  if (!is.null(ds_errors)) {
    warnings <- paste0("Server errors: ",
      paste(names(ds_errors), ds_errors, sep = ": ", collapse = "; "))
  }
  if (scope == "pooled" && length(raw) > 0) {
    pool_out <- .pool_result(raw, "crosstab", pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
  }

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy, warnings = warnings))
}

#' Disclosure-safe two-by-two comorbidity (person co-occurrence)
#'
#' @description
#' A thin wrapper over \code{\link{ds.omop.crosstab}} that cross-tabulates the
#' person-level presence/absence of two concepts as a 2x2 table, using the
#' SAME server-side \code{omopCrossTabDS} machinery (primary + iterative
#' complementary suppression, no exact margins). Because counting and
#' suppression happen server-side, suppressed cells cannot be backed out from
#' margins client-side.
#'
#' @section Comorbidity is descriptive, not inferential:
#' This reports raw person-overlap between two conditions. It is NOT an
#' adjusted measure of association and does not control for confounders such
#' as age. For a genuine multivariable comorbidity model, route to
#' \code{\link[dsBaseClient]{ds.glm}}.
#'
#' @param conceptA Integer; the first concept ID (row presence axis).
#' @param conceptB Integer; the second concept ID (column presence axis).
#' @param tableA Character; the CDM table holding \code{conceptA} (default:
#'   \code{"condition_occurrence"}).
#' @param tableB Character; the CDM table holding \code{conceptB} (default:
#'   same as \code{tableA}).
#' @param ... Additional arguments forwarded to \code{\link{ds.omop.crosstab}}
#'   (e.g., \code{cohort_table}, \code{scope}, \code{pooling_policy},
#'   \code{symbol}, \code{conns}, \code{execute}).
#' @return A \code{dsomop_result} object (see \code{\link{ds.omop.crosstab}}).
#' @examples
#' \dontrun{
#' cm <- ds.omop.comorbidity(316866, 201826)  # hypertension x type 2 diabetes
#' cm$per_site
#' }
#' @export
ds.omop.comorbidity <- function(conceptA, conceptB,
                                tableA = "condition_occurrence",
                                tableB = tableA, ...) {
  ds.omop.crosstab(
    table = tableA,
    row = "condition_concept_id",
    col = "condition_concept_id",
    row_concept_id = conceptA,
    col_concept_id = conceptB,
    ...)
}
