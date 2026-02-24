# ==============================================================================
# dsOMOPClient v2 - Exploration Wrappers (OMOP Studio)
# ==============================================================================

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
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.concept.prevalence <- function(table, concept_col = NULL,
                                        metric = "persons", top_n = 50,
                                        cohort_table = NULL, window = NULL,
                                        symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopConceptPrevalenceDS", session$res_symbol,
                table, concept_col, metric, as.integer(top_n),
                cohort_table, window)
  )
}

#' Get a safe numeric histogram
#'
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param bins Integer; number of bins
#' @param cohort_table Character; cohort temp table (NULL)
#' @param window List with start/end dates (NULL)
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.value.histogram <- function(table, value_col, bins = 20L,
                                     cohort_table = NULL, window = NULL,
                                     symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopNumericHistogramDS", session$res_symbol,
                table, value_col, as.integer(bins),
                cohort_table, window)
  )
}

#' Get safe numeric quantiles
#'
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param probs Numeric vector; quantile probabilities
#' @param cohort_table Character; cohort temp table (NULL)
#' @param window List with start/end dates (NULL)
#' @param rounding Integer; decimal places
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.value.quantiles <- function(table, value_col,
                                     probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                     cohort_table = NULL, window = NULL,
                                     rounding = 2L,
                                     symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopNumericQuantilesDS", session$res_symbol,
                table, value_col, probs,
                cohort_table, window, as.integer(rounding))
  )
}

#' Get date counts by time period
#'
#' @param table Character; table name
#' @param date_col Character; date column (NULL = auto-detect)
#' @param granularity Character; "year", "quarter", or "month"
#' @param cohort_table Character; cohort temp table (NULL)
#' @param window List with start/end dates (NULL)
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.date.counts <- function(table, date_col = NULL,
                                 granularity = "year",
                                 cohort_table = NULL, window = NULL,
                                 symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopDateCountsDS", session$res_symbol,
                table, date_col, granularity,
                cohort_table, window)
  )
}

#' Get concept drilldown profile
#'
#' Returns a full drilldown profile for a single concept within a table,
#' including summary stats, numeric distribution, categorical values,
#' date coverage, and missingness.
#'
#' @param table Character; table name
#' @param concept_id Integer; concept ID to drill into
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) with drilldown results
#' @export
ds.omop.concept.drilldown <- function(table, concept_id,
                                       symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopConceptDrilldownDS", session$res_symbol,
                table, as.integer(concept_id))
  )
}

#' Locate concept across all CDM tables
#'
#' Searches all CDM tables with concept columns and returns a presence
#' matrix showing where given concept IDs appear.
#'
#' @param concept_ids Integer vector; concept IDs to locate
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.concept.locate <- function(concept_ids,
                                    symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopLocateConceptDS", session$res_symbol,
                as.integer(concept_ids))
  )
}
