# ==============================================================================
# dsOMOPClient v2 - Schema Browsing (Dictionary)
# ==============================================================================
# Replaces catalog.R with blueprint annotations.
# ==============================================================================

#' List tables in the OMOP CDM database
#'
#' @param schema_category Character; filter by "CDM", "Vocabulary", "Results"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames with table metadata
#' @export
ds.omop.tables <- function(schema_category = NULL,
                           symbol = "omop",
                           conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  result <- DSI::datashield.aggregate(
    conns,
    expr = call("omopListTablesDS", session$res_symbol)
  )

  if (!is.null(schema_category)) {
    result <- lapply(result, function(df) {
      if (is.data.frame(df) && "schema_category" %in% names(df)) {
        df[tolower(df$schema_category) == tolower(schema_category), ,
           drop = FALSE]
      } else {
        df
      }
    })
  }

  result
}

#' List columns in a table
#'
#' @param table Character; table name
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames with column metadata
#' @export
ds.omop.columns <- function(table, symbol = "omop",
                            conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopListColumnsDS", session$res_symbol, table)
  )
}

#' Get the join relationship graph
#'
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.joins <- function(symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopRelationshipGraphDS", session$res_symbol)
  )
}

#' Compare schemas across servers
#'
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return List with common_tables, server_only, column_diffs
#' @export
ds.omop.compare <- function(symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  caps <- session$capabilities

  if (is.null(caps) || length(caps) < 2) {
    return(list(
      common_tables = if (!is.null(caps))
        caps[[1]]$tables else character(0),
      server_only = list(),
      column_diffs = list(),
      message = "Need 2+ servers for comparison."
    ))
  }

  all_tables <- lapply(caps, function(c) c$tables)
  common <- Reduce(intersect, all_tables)
  server_only <- lapply(
    stats::setNames(names(caps), names(caps)),
    function(srv) setdiff(all_tables[[srv]], common)
  )
  server_only <- server_only[
    vapply(server_only, length, integer(1)) > 0]

  col_diffs <- list()
  for (tbl in common) {
    tryCatch({
      cols_per_server <- ds.omop.columns(
        tbl, symbol = symbol, conns = conns)
      all_col_names <- lapply(cols_per_server, function(df) {
        if (is.data.frame(df)) df$column_name else character(0)
      })
      common_cols <- Reduce(intersect, all_col_names)
      diff_cols <- lapply(
        stats::setNames(names(all_col_names), names(all_col_names)),
        function(srv) setdiff(all_col_names[[srv]], common_cols)
      )
      diff_cols <- diff_cols[
        vapply(diff_cols, length, integer(1)) > 0]
      if (length(diff_cols) > 0) col_diffs[[tbl]] <- diff_cols
    }, error = function(e) NULL)
  }

  list(
    common_tables = common,
    server_only = server_only,
    column_diffs = col_diffs
  )
}

#' Get a full schema snapshot
#'
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) with tables, cdm_info, edges
#' @export
ds.omop.snapshot <- function(symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  caps <- DSI::datashield.aggregate(
    conns,
    expr = call("omopGetCapabilitiesDS", session$res_symbol)
  )

  graph <- DSI::datashield.aggregate(
    conns,
    expr = call("omopRelationshipGraphDS", session$res_symbol)
  )

  lapply(
    stats::setNames(names(caps), names(caps)),
    function(srv) {
      list(
        tables = caps[[srv]]$tables,
        cdm_info = caps[[srv]]$cdm_info,
        edges = graph[[srv]]
      )
    }
  )
}
