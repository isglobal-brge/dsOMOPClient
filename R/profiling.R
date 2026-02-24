# ==============================================================================
# dsOMOPClient v2 - Safe Data Summary Wrappers
# ==============================================================================

#' Get table-level statistics
#'
#' @param table Character; table name
#' @param stats Character vector; which stats to include
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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
#' @param table Character; table name
#' @param column Character; column name
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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
#' @param table Character; table name
#' @param columns Character vector; columns to check
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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
#' @param table Character; table name
#' @param column Character; column name
#' @param top_n Integer; number of top values
#' @param scope Character; "per_site" or "pooled"
#' @param pooling_policy Character; "strict" or "pooled_only_ok"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
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
