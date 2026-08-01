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

  result <- .ds_safe_aggregate(
    conns,
    expr = call("omopListTablesDS", session$res_symbol)
  )

  if (!is.null(schema_category)) {
    result_attrs <- attributes(result)
    result <- lapply(result, function(df) {
      if (is.data.frame(df) && "schema_category" %in% names(df)) {
        df[tolower(df$schema_category) == tolower(schema_category), ,
           drop = FALSE]
      } else {
        df
      }
    })
    for (attribute_name in setdiff(names(result_attrs), "names")) {
      attr(result, attribute_name) <- result_attrs[[attribute_name]]
    }
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

  .ds_safe_aggregate(
    conns,
    expr = call("omopListColumnsDS", session$res_symbol, table)
  )
}

#' Get the join relationship graph
#'
#' @description
#' Retrieves the join relationship graph for the OMOP CDM schema from
#' each connected server. The graph describes standard OMOP relationships
#' (e.g., via \code{person_id}, \code{visit_occurrence_id}, or concept
#' foreign keys). It is an introspection aid; current recipe/plan execution does
#' not consume it to invent arbitrary joins automatically.
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

  .ds_safe_aggregate(
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
#' @param tables Optional character vector limiting column introspection to the
#'   tables relevant to a plan. Table presence is still compared globally.
#' @return A list with schema components including \code{servers} (the exact
#'   nodes compared), \code{common_tables} (character
#'   vector of table names present on all servers), \code{server_only}
#'   (named list of tables unique to each server), and
#'   \code{column_diffs} (named list of per-table column differences), plus
#'   \code{common_columns} (the columns present on every server for each common
#'   table with compatible type families), \code{common_column_types} (their
#'   canonical type families), \code{column_type_diffs} (per-table type-family
#'   mismatches), and \code{column_errors} (named character vector of tables
#'   whose columns could not be inspected). \code{semantic_versions} records
#'   the reported CDM, dsOMOP specification and vocabulary versions for each
#'   node. An empty \code{column_errors} means that all requested common-table
#'   column contracts were established successfully.
#' @examples
#' \dontrun{
#' diff <- ds.omop.compare()
#' diff$common_tables
#' diff$server_only
#' diff$column_diffs
#' }
#' @export
ds.omop.compare <- function(symbol = "omop", conns = NULL, tables = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  caps <- session$capabilities
  if (!is.null(tables)) {
    if (!is.character(tables) || anyNA(tables) || any(!nzchar(tables))) {
      stop("tables must be NULL or a non-missing character vector.",
           call. = FALSE)
    }
    tables <- unique(tolower(tables))
  }
  expected_servers <- names(conns)
  if (is.null(expected_servers) || length(expected_servers) == 0L) {
    expected_servers <- names(caps)
  }

  missing_capabilities <- setdiff(expected_servers, names(caps))
  if (length(missing_capabilities) > 0L) {
    return(list(
      servers = expected_servers,
      common_tables = character(0),
      server_only = list(),
      column_diffs = list(),
      common_columns = list(),
      common_column_types = list(),
      column_type_diffs = list(),
      semantic_versions = list(),
      column_errors = c(
        capabilities = paste0(
          "Missing capability metadata for: ",
          paste(missing_capabilities, collapse = ", ")
        )
      ),
      message = "Schema comparison is incomplete."
    ))
  }
  if (length(expected_servers) > 0L) {
    caps <- caps[expected_servers]
  }
  semantic_versions <- lapply(caps, function(cap) {
    scalar <- function(x) {
      if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(as.character(x))) {
        return(NA_character_)
      }
      as.character(x)
    }
    list(
      cdm_version = scalar(cap$cdm_info$cdm_version),
      spec_version = scalar(cap$spec_version),
      vocabulary_version = scalar(cap$cdm_info$vocabulary_version)
    )
  })

  if (is.null(caps) || length(caps) < 2) {
    return(list(
      servers = names(caps),
      common_tables = if (!is.null(caps) && length(caps) > 0L)
        caps[[1]]$tables else character(0),
      server_only = list(),
      column_diffs = list(),
      common_columns = list(),
      common_column_types = list(),
      column_type_diffs = list(),
      semantic_versions = semantic_versions,
      column_errors = character(0),
      message = "Need 2+ servers for comparison."
    ))
  }

  all_tables <- lapply(caps, function(c) sort(unique(tolower(c$tables))))
  common <- Reduce(intersect, all_tables)
  server_only <- lapply(
    stats::setNames(names(caps), names(caps)),
    function(srv) setdiff(all_tables[[srv]], common)
  )
  server_only <- server_only[
    vapply(server_only, length, integer(1)) > 0]

  col_diffs <- list()
  common_columns <- list()
  common_column_types <- list()
  column_type_diffs <- list()
  column_errors <- character(0)
  inspect_tables <- if (is.null(tables)) common else intersect(common, tables)
  for (tbl in inspect_tables) {
    tryCatch({
      cols_per_server <- ds.omop.columns(
        tbl, symbol = symbol, conns = conns)
      aggregate_errors <- attr(cols_per_server, "ds_errors")
      missing_servers <- setdiff(expected_servers, names(cols_per_server))
      if (length(aggregate_errors) > 0L || length(missing_servers) > 0L) {
        details <- character(0)
        if (length(aggregate_errors) > 0L) {
          details <- c(details, paste(
            names(aggregate_errors), unlist(aggregate_errors, use.names = FALSE),
            sep = ": "
          ))
        }
        missing_without_error <- setdiff(missing_servers,
                                         names(aggregate_errors))
        if (length(missing_without_error) > 0L) {
          details <- c(details, paste0(
            missing_without_error, ": no introspection result"
          ))
        }
        stop(paste(details, collapse = "; "), call. = FALSE)
      }
      cols_per_server <- cols_per_server[expected_servers]
      invalid_results <- expected_servers[!vapply(
        cols_per_server,
        function(df) is.data.frame(df) &&
          "column_name" %in% names(df) &&
          any(c("cdm_datatype", "db_datatype", "data_type") %in% names(df)) &&
          !anyNA(df$column_name) &&
          !any(!nzchar(as.character(df$column_name))) &&
          !anyDuplicated(tolower(as.character(df$column_name))),
        logical(1)
      )]
      if (length(invalid_results) > 0L) {
        stop("Invalid column metadata from: ",
             paste(invalid_results, collapse = ", "), call. = FALSE)
      }
      all_col_names <- lapply(cols_per_server, function(df) {
        sort(unique(tolower(as.character(df$column_name))))
      })
      common_by_name <- Reduce(intersect, all_col_names)
      type_maps <- lapply(cols_per_server, function(df) {
        columns <- tolower(as.character(df$column_name))
        actual <- if ("db_datatype" %in% names(df)) {
          df$db_datatype
        } else if ("data_type" %in% names(df)) {
          df$data_type
        } else {
          df$cdm_datatype
        }
        families <- .omop_column_type_family(actual)
        stats::setNames(families, columns)
      })
      cdm_type_maps <- lapply(seq_along(cols_per_server), function(i) {
        df <- cols_per_server[[i]]
        columns <- tolower(as.character(df$column_name))
        expected_families <- if ("cdm_datatype" %in% names(df)) {
          .omop_column_type_family(df$cdm_datatype)
        } else {
          unname(type_maps[[i]])
        }
        # Authorized extension columns and introspection-only CDM versions have
        # no OHDSI datatype declaration. Their actual DB family is the only
        # executable contract and must still agree across every server.
        missing_expected <- is.na(expected_families) |
          !nzchar(expected_families)
        expected_families[missing_expected] <-
          unname(type_maps[[i]])[missing_expected]
        stats::setNames(expected_families, columns)
      })
      type_mismatches <- list()
      for (column in common_by_name) {
        actual_families <- vapply(type_maps, function(types) {
          unname(types[[column]])
        }, character(1))
        cdm_families <- vapply(cdm_type_maps, function(types) {
          unname(types[[column]])
        }, character(1))
        invalid_implementation <- actual_families != cdm_families
        if (anyNA(actual_families) || anyNA(cdm_families) ||
            any(!nzchar(actual_families)) || any(!nzchar(cdm_families)) ||
            any(invalid_implementation) ||
            length(unique(actual_families)) > 1L ||
            length(unique(cdm_families)) > 1L) {
          display <- actual_families
          display[invalid_implementation & !is.na(invalid_implementation)] <-
            paste0("cdm=", cdm_families[invalid_implementation &
                                          !is.na(invalid_implementation)],
                   ";db=", actual_families[invalid_implementation &
                                             !is.na(invalid_implementation)])
          type_mismatches[[column]] <- display
        }
      }
      common_cols <- setdiff(common_by_name, names(type_mismatches))
      common_columns[[tbl]] <- common_cols
      common_column_types[[tbl]] <- stats::setNames(
        vapply(common_cols, function(column) {
          unname(cdm_type_maps[[1L]][[column]])
        }, character(1)),
        common_cols
      )
      if (length(type_mismatches) > 0L) {
        column_type_diffs[[tbl]] <- type_mismatches
      }
      diff_cols <- lapply(
        stats::setNames(names(all_col_names), names(all_col_names)),
        function(srv) sort(setdiff(all_col_names[[srv]], common_by_name))
      )
      diff_cols <- diff_cols[
        vapply(diff_cols, length, integer(1)) > 0]
      if (length(diff_cols) > 0) col_diffs[[tbl]] <- diff_cols
    }, error = function(e) {
      common_columns[[tbl]] <<- character(0)
      column_errors[[tbl]] <<- conditionMessage(e)
    })
  }

  list(
    servers = expected_servers,
    common_tables = common,
    server_only = server_only,
    column_diffs = col_diffs,
    common_columns = common_columns,
    common_column_types = common_column_types,
    column_type_diffs = column_type_diffs,
    semantic_versions = semantic_versions,
    column_errors = column_errors
  )
}

.omop_column_type_family <- function(data_type) {
  value <- tolower(trimws(as.character(data_type)))
  value[is.na(data_type) | !nzchar(value)] <- NA_character_
  result <- value
  result[grepl("blob|binary|bytea|raw", value)] <- "binary"
  result[grepl("char|text|clob|json|uuid|enum", value)] <- "character"
  result[grepl("int|numeric|decimal|number|double|float|real|serial", value)] <-
    "numeric"
  result[grepl("bool|bit", value)] <- "logical"
  result[grepl("date|time|interval", value)] <- "temporal"
  result
}

#' Get a full schema snapshot
#'
#' @description
#' Retrieves a comprehensive schema snapshot from each connected server,
#' combining capabilities metadata (available tables, CDM version info)
#' with the join relationship graph into a single structure. This
#' provides a complete picture of the database schema that can be cached
#' client-side and used to drive headless schema exploration and query
#' building.
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

  caps <- .ds_safe_aggregate(
    conns,
    expr = call("omopGetCapabilitiesDS", session$res_symbol)
  )

  graph <- .ds_safe_aggregate(
    conns,
    expr = call("omopRelationshipGraphDS", session$res_symbol)
  )

  complete_servers <- intersect(names(caps), names(graph))
  result <- lapply(
    stats::setNames(complete_servers, complete_servers),
    function(srv) {
      list(
        tables = caps[[srv]]$tables,
        cdm_info = caps[[srv]]$cdm_info,
        edges = graph[[srv]]
      )
    }
  )
  cap_errors <- attr(caps, "ds_errors") %||% list()
  graph_errors <- attr(graph, "ds_errors") %||% list()
  incomplete <- setdiff(names(conns), complete_servers)
  if (length(incomplete) > 0L) {
    errors <- stats::setNames(lapply(incomplete, function(server) {
      details <- c(
        if (!is.null(cap_errors[[server]])) {
          paste0("capabilities: ", cap_errors[[server]])
        },
        if (!is.null(graph_errors[[server]])) {
          paste0("relationships: ", graph_errors[[server]])
        }
      )
      if (length(details) == 0L) "incomplete schema snapshot" else
        paste(details, collapse = "; ")
    }), incomplete)
    attr(result, "ds_errors") <- errors
  }
  result
}
