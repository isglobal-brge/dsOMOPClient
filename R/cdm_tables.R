# OMOP table metadata used by recipe compilation (design-2.10.3 section 8,
# defects F-1 and F-2).
#
# inst/ohdsi/cdm_columns.csv lists the columns of every CDM 5.3 / 5.4 table,
# derived from the server's vendored OHDSI field metadata and kept
# byte-identical with the server's copy (the server suite pins both the
# derivation and the identity). inst/ohdsi/roles-5.x.csv are the role records
# of the server's role registry (design 6.2).

.cdm_tables_cache <- new.env(parent = emptyenv())

.cdm_columns <- function() {
  cached <- .cdm_tables_cache$columns
  if (!is.null(cached)) return(cached)
  path <- system.file("ohdsi", "cdm_columns.csv", package = "dsOMOPClient")
  if (!nzchar(path)) stop("Packaged CDM column metadata is missing.", call. = FALSE)
  cols <- utils::read.csv(path, colClasses = "character",
                          stringsAsFactors = FALSE)
  assign("columns", cols, envir = .cdm_tables_cache)
  cols
}

#' Columns of one OMOP CDM table (CDM 5.3 and 5.4)
#'
#' @param table Table name (case-insensitive).
#' @return Character vector (empty for a table the CDM does not define).
#' @keywords internal
.cdm_table_columns <- function(table) {
  if (!is.character(table) || length(table) != 1L || is.na(table)) {
    return(character(0))
  }
  cols <- .cdm_columns()
  cols$column_name[cols$table_name == tolower(table)]
}

#' Domain concept column of an event table, from the role registry
#'
#' @param table Table name (case-insensitive), or NULL.
#' @return The domain concept column (CDM 5.4 record first, then 5.3), or
#'   NULL when the table is unknown or has none.
#' @keywords internal
.role_domain_concept <- function(table) {
  if (!is.character(table) || length(table) != 1L || is.na(table)) return(NULL)
  t <- tolower(table)
  for (version in c("5.4", "5.3")) {
    key <- paste0("roles_", version)
    rows <- .cdm_tables_cache[[key]]
    if (is.null(rows)) {
      path <- system.file("ohdsi", paste0("roles-", version, ".csv"),
                          package = "dsOMOPClient")
      if (!nzchar(path)) next
      rows <- utils::read.csv(path, colClasses = "character",
                              na.strings = character(0),
                              stringsAsFactors = FALSE)
      assign(key, rows, envir = .cdm_tables_cache)
    }
    hit <- rows$domain_concept_col[rows$table_name == t &
                                     rows$event_source == "TRUE"]
    if (length(hit) == 1L && nzchar(hit)) return(hit)
  }
  NULL
}

# Every column a compiled row-filter tree references.
.row_filter_vars <- function(tree) {
  if (is.null(tree) || !is.list(tree) || length(tree) == 0L) return(character(0))
  for (group in c("and", "or")) {
    if (!is.null(tree[[group]])) {
      return(unique(unlist(lapply(tree[[group]], .row_filter_vars),
                           use.names = FALSE)))
    }
  }
  if (!is.null(tree$not)) return(.row_filter_vars(tree$not))
  var <- tree$var
  if (is.character(var) && length(var) == 1L) tolower(var) else character(0)
}

#' Refuse a recipe-level row filter on an output whose table lacks a column
#'
#' Defect F-2: a recipe-level row filter is attached only to outputs whose
#' event table has every column the filter references; otherwise compilation
#' stops naming the output and the missing column. The table-agnostic
#' sentinels (\code{start_date}, \code{end_date}, \code{concept_id}) are
#' resolved per table by the server; a column the CDM does not define at all
#' (a site extension) is left to the server's own validation.
#'
#' @param tree Compiled row-filter tree for the output's table.
#' @param table The output's event table.
#' @param output_name The plan output name.
#' @return \code{TRUE} invisibly.
#' @keywords internal
.assert_row_filter_columns <- function(tree, table, output_name) {
  if (is.null(tree) || is.null(table)) return(invisible(TRUE))
  table_cols <- .cdm_table_columns(table)
  if (length(table_cols) == 0L) return(invisible(TRUE))
  known <- unique(.cdm_columns()$column_name)
  sentinels <- c("start_date", "end_date", "concept_id")
  for (var in .row_filter_vars(tree)) {
    if (var %in% sentinels || var %in% table_cols || !var %in% known) next
    stop("Recipe row filter column '", var, "' does not exist on table '",
         table, "' of output '", output_name, "'. Put the predicate on an ",
         "event of that table (omop_event(where = ...)) or restrict the filter ",
         "to outputs over tables that have the column.", call. = FALSE)
  }
  invisible(TRUE)
}
