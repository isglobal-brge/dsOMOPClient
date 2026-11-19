# Module: Data Profiling
# Client-side wrappers for table statistics, column statistics, domain
# coverage, missingness, and value counts.

#' Get table-level statistics
#'
#' @description
#' Returns row counts and person counts for the specified OMOP CDM table.
#' Results are disclosure-controlled on the server side (counts below the
#' minimum cell threshold are suppressed, surviving counts are banded) and
#' returned per server and, by default, pooled (summed) across servers, as
#' \code{ds.dim} does.
#'
#' @param table Character; the CDM table name (e.g.,
#'   \code{"condition_occurrence"}, \code{"person"}).
#' @param stats Character vector; which statistics to compute:
#'   \code{"rows"} (total row count), \code{"persons"} (distinct person
#'   count) and \code{"date_range"} (first and last supported month).
#'   Default: \code{c("rows", "persons")}.
#' @param scope Deprecated alias of \code{type}: \code{"per_site"} means
#'   \code{type = "split"}, \code{"pooled"} means \code{type = "both"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}. See \link{dsomop_conventions}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param execute Logical; if \code{FALSE}, return a dry-run result
#'   containing only the generated call code (default: \code{TRUE}).
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. See \link{dsomop_conventions}.
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. Each \code{$per_site} entry is a list with
#'   the banded \code{rows} and \code{persons} counts (a count the server
#'   suppressed is absent) and, when requested, \code{date_range}
#'   (\code{column}, \code{min_month}, \code{max_month}). \code{$pooled} has
#'   the same fields: counts summed over the servers and the date range
#'   spanning them.
#' @examples
#' \dontrun{
#' result <- ds.omop.table.stats("condition_occurrence")
#' result$per_site$server1
#' result$pooled$persons
#'
#' ds.omop.table.stats("drug_exposure", type = "split")
#' }
#' @export
ds.omop.table.stats <- function(table,
                                stats = c("rows", "persons"),
                                scope = c("per_site", "pooled"),
                                pooling_policy = "strict",
                                symbol = "omop",
                                conns = NULL,
                                execute = TRUE,
                                type = "both",
                                datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.table.stats")
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing,
    type_missing = missing(type), default_type = "both",
    fn = "ds.omop.table.stats"
  )
  pooling_policy <- .pooling_policy_arg(pooling_policy)

  code <- .build_code("ds.omop.table.stats",
    table = table, stats = stats,
    pooling_policy = .code_policy(pooling_policy),
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopTableStatsDS", session$res_symbol,
                table, .ds_encode(stats))
  )

  .reader_result(raw, "table_stats", result_type, pooling_policy, code,
    "No complete disclosure-safe pooled table statistics were available.")
}

#' Get column-level statistics
#'
#' @description
#' Returns statistics for a single column in an OMOP CDM table: banded row,
#' person and missing counts, the number of distinct values, and (for numeric
#' columns) the mean and standard deviation of one value per person. All
#' counts are disclosure-controlled on the server side. Like \code{ds.mean},
#' the default view is per server.
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
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side cohort table name), or NULL.
#' @inheritParams ds.omop.table.stats
#' @param type Result view: \code{"split"} (default), \code{"both"}, or
#'   \code{"combine"}. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. Each \code{$per_site} entry is a list with
#'   \code{n_total}, \code{n_missing}, \code{n_distinct} and, for person
#'   tables, \code{n_persons} (counts banded; a suppressed value is
#'   \code{NA}), plus \code{mean} and \code{sd} for numeric columns.
#'   \code{$pooled} sums the counts, weights the mean by the contributing
#'   persons and pools the SD; the distinct count is not additive and is not
#'   pooled.
#' @examples
#' \dontrun{
#' col_info <- ds.omop.column.stats("measurement", "value_as_number")
#' col_info$per_site$server1
#' }
#' @export
ds.omop.column.stats <- function(table, column, concept_id = NULL,
                                 cohort = NULL,
                                 scope = c("per_site", "pooled"),
                                 pooling_policy = "strict",
                                 symbol = "omop",
                                 conns = NULL,
                                 execute = TRUE,
                                 type = "split",
                                 datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.column.stats")
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing,
    type_missing = missing(type), default_type = "split",
    fn = "ds.omop.column.stats"
  )
  pooling_policy <- .pooling_policy_arg(pooling_policy)
  cohort_scope <- .cohort_scope_arg(cohort)

  code <- .build_code("ds.omop.column.stats",
    table = table, column = column, concept_id = concept_id,
    cohort = cohort_scope, pooling_policy = .code_policy(pooling_policy),
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopColumnStatsDS", session$res_symbol,
                table, column, concept_id = concept_id,
                cohort = cohort_scope)
  )

  .reader_result(raw, "column_stats", result_type, pooling_policy, code,
    "No complete disclosure-safe pooled column statistics were available.")
}

#' Get cross-table domain coverage
#'
#' @description
#' Provides a high-level overview of data coverage across the OMOP CDM
#' clinical tables: for each table with persons, the banded number of
#' distinct persons. Tables below the disclosure threshold are omitted.
#'
#' @inheritParams ds.omop.table.stats
#' @return A \code{dsomop_result}. Each \code{$per_site} entry is a data
#'   frame with \code{table_name} and \code{n_persons} (banded).
#'   \code{$pooled} sums \code{n_persons} per table; a table absent on a
#'   server is not treated as zero.
#' @examples
#' \dontrun{
#' coverage <- ds.omop.domain.coverage()
#' coverage$pooled
#' }
#' @export
ds.omop.domain.coverage <- function(scope = c("per_site", "pooled"),
                                    pooling_policy = "strict",
                                    symbol = "omop",
                                    conns = NULL,
                                    execute = TRUE,
                                    type = "both",
                                    datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.domain.coverage")
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing,
    type_missing = missing(type), default_type = "both",
    fn = "ds.omop.domain.coverage"
  )
  pooling_policy <- .pooling_policy_arg(pooling_policy)

  code <- .build_code("ds.omop.domain.coverage",
    pooling_policy = .code_policy(pooling_policy),
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopDomainCoverageDS", session$res_symbol)
  )

  .reader_result(raw, "domain_coverage", result_type, pooling_policy, code,
    "No complete disclosure-safe pooled domain coverage was available.")
}

#' Get missingness rates for columns
#'
#' @description
#' Computes the proportion of NULL/missing values for each column (or a
#' subset of columns) in an OMOP CDM table. For person tables a person is
#' missing a column when none of their records carries a value. Results are
#' disclosure-controlled: a rate whose missing or non-missing group is below
#' the threshold is \code{NA}.
#'
#' @param table Character; the CDM table name (e.g.,
#'   \code{"measurement"}, \code{"observation"}).
#' @param columns Character vector; specific column names to check, or
#'   NULL to check all columns in the table (default: NULL).
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side cohort table name), or NULL.
#' @inheritParams ds.omop.column.stats
#' @return A \code{dsomop_result}. Each \code{$per_site} entry is a data
#'   frame with \code{column_name} and \code{missing_rate}; dsOMOP 2.10.9
#'   servers add the banded counts behind the rate, \code{n_total} and
#'   \code{n_missing}. \code{$pooled} has \code{column_name}, \code{n_total},
#'   \code{n_missing} and \code{missing_rate} = summed \code{n_missing} /
#'   summed \code{n_total}. Older servers return rates only; their results
#'   are not pooled and \code{meta$pooling_reason} says so.
#' @examples
#' \dontrun{
#' miss <- ds.omop.missingness("measurement",
#'                              columns = c("value_as_number",
#'                                          "value_as_concept_id"),
#'                              type = "both")
#' miss$pooled
#' }
#' @export
ds.omop.missingness <- function(table, columns = NULL, cohort = NULL,
                                scope = c("per_site", "pooled"),
                                pooling_policy = "strict",
                                symbol = "omop",
                                conns = NULL,
                                execute = TRUE,
                                type = "split",
                                datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.missingness")
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing,
    type_missing = missing(type), default_type = "split",
    fn = "ds.omop.missingness"
  )
  pooling_policy <- .pooling_policy_arg(pooling_policy)
  cohort_scope <- .cohort_scope_arg(cohort)

  code <- .build_code("ds.omop.missingness",
    table = table, columns = columns, cohort = cohort_scope,
    pooling_policy = .code_policy(pooling_policy),
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopMissingnessDS", session$res_symbol,
                table, .ds_encode(columns), cohort = cohort_scope)
  )

  .reader_result(raw, "missingness", result_type, pooling_policy, code,
    "No complete disclosure-safe pooled missingness result was available.")
}

#' Get value frequencies for a column
#'
#' @description
#' Returns the top-N most frequent distinct values for a column in an
#' OMOP CDM table, along with their counts. This is useful for profiling
#' categorical or low-cardinality columns such as \code{type_concept_id}
#' or \code{unit_concept_id}. Values below the disclosure threshold are
#' left out, surviving counts are banded, and concept names are resolved
#' for concept columns.
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
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side cohort table name), or NULL.
#' @inheritParams ds.omop.table.stats
#' @return A \code{dsomop_result}. Each \code{$per_site} entry is a data
#'   frame with \code{value}, \code{n} (banded records), \code{n_persons}
#'   (banded distinct persons, person tables) and \code{concept_name}
#'   (concept columns). \code{$pooled} sums \code{n} and \code{n_persons} per
#'   value over the servers; a value must be released by every server.
#' @examples
#' \dontrun{
#' vc <- ds.omop.value.counts("condition_occurrence",
#'                             "condition_type_concept_id",
#'                             top_n = 10)
#' vc$pooled
#' }
#' @export
ds.omop.value.counts <- function(table, column, top_n = 20,
                                 concept_id = NULL, cohort = NULL,
                                 scope = c("per_site", "pooled"),
                                 pooling_policy = "strict",
                                 symbol = "omop",
                                 conns = NULL,
                                 execute = TRUE,
                                 type = "both",
                                 datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.value.counts")
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing,
    type_missing = missing(type), default_type = "both",
    fn = "ds.omop.value.counts"
  )
  pooling_policy <- .pooling_policy_arg(pooling_policy)
  cohort_scope <- .cohort_scope_arg(cohort)

  code <- .build_code("ds.omop.value.counts",
    table = table, column = column, top_n = top_n,
    concept_id = concept_id, cohort = cohort_scope,
    pooling_policy = .code_policy(pooling_policy),
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopValueCountsDS", session$res_symbol,
                table, column, as.integer(top_n),
                concept_id = concept_id, cohort = cohort_scope)
  )

  .reader_result(raw, "value_counts", result_type, pooling_policy, code,
    "No complete disclosure-safe pooled value counts were available.")
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
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side cohort table name), or NULL.
#'   Takes precedence over \code{cohort_table}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}. Under both policies a cell absent or suppressed
#'   on any contributing server is \code{NA} in the pooled table;
#'   \code{"pooled_only_ok"} only leaves out servers whose whole cross-tab
#'   was suppressed or empty. See \link{dsomop_conventions}.
#' @inheritParams ds.omop.table.stats
#' @return A \code{dsomop_result}. Each \code{$per_site} entry is the server's
#'   cross-tab: \code{row_col}, \code{col_col}, \code{count_mode},
#'   \code{row_levels}, \code{col_levels} and \code{counts} (banded matrix,
#'   \code{NA} = suppressed), or for a stratified call
#'   \code{stratified = TRUE}, \code{stratify_by} and \code{strata} (one such
#'   slice per level). \code{$pooled} has the same shape with cell-wise sums
#'   and a \code{suppressed} flag.
#' @examples
#' \dontrun{
#' ct <- ds.omop.crosstab("person", "gender_concept_id", "race_concept_id")
#' ct$pooled$counts
#' }
#' @export
ds.omop.crosstab <- function(table, row, col, by = "persons",
                             row_concept_id = NULL, col_concept_id = NULL,
                             cohort_table = NULL, stratify_by = NULL,
                             cohort = NULL,
                             scope = c("per_site", "pooled"),
                             pooling_policy = "strict",
                             symbol = "omop",
                             conns = NULL,
                             execute = TRUE,
                             type = "both",
                             datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.crosstab")
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing,
    type_missing = missing(type), default_type = "both",
    fn = "ds.omop.crosstab"
  )
  pooling_policy <- .pooling_policy_arg(pooling_policy)
  cohort_scope <- .cohort_scope_arg(cohort) %||% cohort_table

  code <- .build_code("ds.omop.crosstab",
    table = table, row = row, col = col, by = by,
    row_concept_id = row_concept_id, col_concept_id = col_concept_id,
    cohort_table = cohort_scope, stratify_by = stratify_by,
    pooling_policy = .code_policy(pooling_policy),
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopCrossTabDS", session$res_symbol,
                table, row, col,
                count_mode = by,
                row_concept_ids = row_concept_id,
                col_concept_ids = col_concept_id,
                cohort_table = cohort_scope,
                stratify_by = stratify_by)
  )

  .reader_result(raw, "crosstab", result_type, pooling_policy, code,
    "No complete disclosure-safe pooled cross-tabulation was available.")
}

#' Disclosure-safe two-by-two comorbidity (person co-occurrence)
#'
#' @description
#' A thin wrapper over \code{\link{ds.omop.crosstab}} that cross-tabulates the
#' person-level presence/absence of two concepts of one table as a 2x2 table,
#' using the SAME server-side \code{omopCrossTabDS} machinery (primary +
#' iterative complementary suppression, no exact margins). Because counting
#' and suppression happen server-side, suppressed cells cannot be backed out
#' from margins client-side. Both axes use the table's domain concept column
#' (for example \code{drug_concept_id} for \code{drug_exposure}).
#'
#' @section Comorbidity is descriptive, not inferential:
#' This reports raw person-overlap between two conditions. It is NOT an
#' adjusted measure of association and does not control for confounders such
#' as age. For a genuine multivariable comorbidity model, route to
#' \code{\link[dsBaseClient]{ds.glm}}.
#'
#' @param conceptA Integer; the first concept ID (row presence axis).
#' @param conceptB Integer; the second concept ID (column presence axis).
#' @param tableA Character; the CDM table holding both concepts (default:
#'   \code{"condition_occurrence"}).
#' @param tableB Character; must equal \code{tableA}. The server cross-tab
#'   reads one table, so two concepts of different tables are refused with an
#'   explanation rather than silently counted in \code{tableA}.
#' @param ... Additional arguments forwarded to \code{\link{ds.omop.crosstab}}
#'   (e.g., \code{cohort}, \code{type}, \code{pooling_policy},
#'   \code{symbol}, \code{datasources}, \code{execute}).
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
  if (!is.character(tableA) || length(tableA) != 1L || is.na(tableA) ||
      !is.character(tableB) || length(tableB) != 1L || is.na(tableB)) {
    stop("tableA and tableB must each be one table name.", call. = FALSE)
  }
  if (!identical(tolower(tableA), tolower(tableB))) {
    stop("ds.omop.comorbidity() cross-tabulates two concepts of one table: ",
         "the server cross-tab reads a single table, so tableB must equal ",
         "tableA ('", tableA, "'). For concepts of different tables, build ",
         "a recipe with both as person-level indicators and tabulate them ",
         "with ds.table.", call. = FALSE)
  }
  concept_column <- tryCatch(.plan_domain_concept_column(tableA),
                             error = function(e) NULL)
  if (!is.character(concept_column) || length(concept_column) != 1L) {
    stop("ds.omop.comorbidity() needs a clinical table with a domain concept ",
         "column; '", tableA, "' has none.", call. = FALSE)
  }
  ds.omop.crosstab(
    table = tableA,
    row = concept_column,
    col = concept_column,
    row_concept_id = conceptA,
    col_concept_id = conceptB,
    ...)
}
