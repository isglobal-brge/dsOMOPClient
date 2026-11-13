# Event tables of the role registry (design-2.10.3 section 6.2).
#
# The server's role records ship as inst/ohdsi/roles-5.3.csv and
# roles-5.4.csv; the client vendors byte-identical copies (the server suite
# pins the identity) so client-side validation names the same tables the
# server accepts as event sources (defect B-20).

.role_event_tables_cache <- new.env(parent = emptyenv())

#' Event-source tables of the role registry (every CDM version)
#'
#' @return Character vector of table names.
#' @keywords internal
.role_event_tables <- function() {
  cached <- .role_event_tables_cache$tables
  if (!is.null(cached)) return(cached)
  tables <- character(0)
  for (version in c("5.3", "5.4")) {
    path <- system.file("ohdsi", paste0("roles-", version, ".csv"),
                        package = "dsOMOPClient")
    if (!nzchar(path)) {
      stop("Packaged role registry for CDM ", version, " is missing.",
           call. = FALSE)
    }
    records <- utils::read.csv(path, colClasses = "character",
                               na.strings = character(0),
                               stringsAsFactors = FALSE)
    tables <- c(tables, records$table_name[records$event_source == "TRUE"])
  }
  tables <- unique(tables)
  assign("tables", tables, envir = .role_event_tables_cache)
  tables
}
