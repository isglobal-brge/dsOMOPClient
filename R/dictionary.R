# Module: Data Dictionary
# Client-side wrappers for CDM schema introspection: tables, columns,
# relationships, and capabilities.

#' List tables in the OMOP CDM database
#'
#' @description
#' Queries each connected server for the list of OMOP CDM tables present
#' in the database. Returns metadata including the schema category
#' (\code{"CDM"}, \code{"Vocabulary"}, \code{"Results"}) and whether the
#' table contains a \code{person_id} column. An optional filter allows
#' restricting results to a single schema category.
#'
#' @param schema_category Character; optional filter to restrict results
#'   to a specific category: \code{"CDM"}, \code{"Vocabulary"}, or
#'   \code{"Results"}. NULL returns all tables (default: NULL).
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @return A named list (one element per server) of data frames with
#'   table metadata columns such as \code{table_name},
#'   \code{schema_category}, and \code{has_person_id}.
#' @examples
#' \dontrun{
#' tables <- ds.omop.tables()
#' tables$server1
#'
#' cdm_only <- ds.omop.tables(schema_category = "CDM")
#' }
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
#' @description
#' Queries each connected server for the list of columns present in the
#' specified OMOP CDM table. Returns metadata for each column including
#' the column name, data type, whether the column is nullable, and
#' whether it is a concept ID column or a date column.
#'
#' @param table Character; the CDM table name to introspect (e.g.,
#'   \code{"condition_occurrence"}, \code{"person"}).
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @return A named list (one element per server) of data frames with
#'   column metadata such as \code{column_name}, \code{data_type},
#'   \code{is_nullable}, \code{is_concept}, and \code{is_date}.
#' @examples
#' \dontrun{
#' cols <- ds.omop.columns("condition_occurrence")
#' cols$server1
#' }
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
#' @description
#' Retrieves the join relationship graph for the OMOP CDM schema from
#' each connected server. The graph describes how tables can be joined
#' (e.g., via \code{person_id}, \code{visit_occurrence_id}, or concept
#' foreign keys), which is used by the query builder and extraction
#' pipeline to construct multi-table joins automatically.
#'
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @return A named list (one element per server) of data frames with
#'   edge metadata such as \code{from_table}, \code{to_table},
#'   \code{from_column}, and \code{to_column}.
#' @examples
#' \dontrun{
#' joins <- ds.omop.joins()
#' joins$server1
#' }
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
#' @description
#' Compares the OMOP CDM schemas across all connected servers to identify
#' structural differences. Returns the set of tables common to all
#' servers, tables unique to specific servers, and per-table column
#' differences. This is useful for diagnosing schema mismatches before
#' running pooled analyses. Requires at least two connected servers for
#' meaningful comparison; with a single server, returns that server's
#' tables as the common set.
#'
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @return A list with three components: \code{common_tables} (character
#'   vector of table names present on all servers), \code{server_only}
#'   (named list of tables unique to each server), and
#'   \code{column_diffs} (named list of per-table column differences).
#' @examples
#' \dontrun{
#' diff <- ds.omop.compare()
#' diff$common_tables
#' diff$server_only
#' diff$column_diffs
#' }
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
#' @description
#' Retrieves a comprehensive schema snapshot from each connected server,
#' combining capabilities metadata (available tables, CDM version info)
#' with the join relationship graph into a single structure. This
#' provides a complete picture of the database schema that can be cached
#' client-side for use by the UI and query builder.
#'
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or NULL to use the session default.
#' @return A named list (one element per server), where each element is
#'   a list with \code{tables} (character vector of table names),
#'   \code{cdm_info} (list with CDM version and DBMS details), and
#'   \code{edges} (data frame of join relationships).
#' @examples
#' \dontrun{
#' snap <- ds.omop.snapshot()
#' snap$server1$tables
#' snap$server1$cdm_info
#' snap$server1$edges
#' }
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
