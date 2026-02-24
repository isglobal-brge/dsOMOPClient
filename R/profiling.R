# ==============================================================================
# dsOMOPClient v2 - Safe Data Summary Wrappers
# ==============================================================================

#' Get table-level statistics
#'
#' @param table Character; table name
#' @param stats Character vector; which stats to include
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of statistics
#' @export
ds.omop.table.stats <- function(table,
                                stats = c("rows", "persons"),
                                symbol = "omop",
                                conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopTableStatsDS", session$res_symbol,
                table, stats)
  )
}

#' Get column-level statistics
#'
#' @param table Character; table name
#' @param column Character; column name
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of column statistics
#' @export
ds.omop.column.stats <- function(table, column,
                                 symbol = "omop",
                                 conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopColumnStatsDS", session$res_symbol,
                table, column)
  )
}

#' Get cross-table domain coverage
#'
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.domain.coverage <- function(symbol = "omop",
                                    conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopDomainCoverageDS", session$res_symbol)
  )
}

#' Get missingness rates for columns
#'
#' @param table Character; table name
#' @param columns Character vector; columns to check
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.missingness <- function(table, columns = NULL,
                                symbol = "omop",
                                conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopMissingnessDS", session$res_symbol,
                table, columns)
  )
}

#' Get value frequencies for a column
#'
#' @param table Character; table name
#' @param column Character; column name
#' @param top_n Integer; number of top values
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.value.counts <- function(table, column, top_n = 20,
                                 symbol = "omop",
                                 conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopValueCountsDS", session$res_symbol,
                table, column, as.integer(top_n), TRUE)
  )
}
