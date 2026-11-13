#' Pool a published concept category with its complete level contract
#'
#' Calls the real \code{dsBaseClient::ds.table} on a published in-memory
#' concept factor. Uses the existing disclosure-checked factor harmonisation
#' transport to restore declared categories absent from every site as zero
#' cells, in federation level order. No rows are added or changed. All sites
#' must return valid counts; failed or suppressed tables are never zero-filled.
#'
#' dsBase rebuilds factors from observed values, so a direct \code{ds.table}
#' call can omit globally unused levels. Use this integration when the pooled
#' table must retain the complete declared category contract. Counts exclude
#' actual missing values; a literal label \code{"NA"} remains a category.
#'
#' @param data Character; published server-side data frame symbol.
#' @param column Character; concept factor column in that data frame.
#' @param conns DSI connections for the retained sites. Defaults to the active
#'   DataSHIELD connections.
#' @return A one-dimensional \code{table} of pooled counts, including unused
#'   declared categories with count zero.
#' @export
ds.omop.table <- function(data, column, conns = NULL) {
  for (x in list(data, column)) {
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !isTRUE(.conceptValidNames(x))) {
      stop("data and column must be single valid R names.", call. = FALSE)
    }
  }
  if (!requireNamespace("dsBaseClient", quietly = TRUE)) {
    stop("ds.omop.table requires dsBaseClient.", call. = FALSE)
  }
  if (is.null(conns)) conns <- DSI::datashield.connections_find()
  if (!length(conns) || is.null(names(conns)) || anyDuplicated(names(conns))) {
    stop("Supply named connections for the retained sites.", call. = FALSE)
  }
  reports <- DSI::datashield.aggregate(conns,
    call("omopFactorLevelsDS", as.symbol(data)))
  if (!setequal(names(reports), names(conns)) ||
      any(vapply(reports, is.null, logical(1)))) {
    stop("Incomplete concept level reports; pooled table refused.", call. = FALSE)
  }
  spec <- .conceptUnionReports(reports[names(conns)])
  key <- column
  if (length(spec$names)) key <- names(spec$names)[unlist(spec$names) == column]
  if (length(key) != 1L || is.null(spec$columns[[key]])) {
    stop("column must have a published concept level contract.", call. = FALSE)
  }
  levels <- as.character(unlist(spec$columns[[key]]$levels, use.names = FALSE))
  result <- dsBaseClient::ds.table(rvar = paste0(data, "$", column),
                                  useNA = "no", datasources = conns)
  .conceptCompleteTable(result, levels, paste0(data, "$", column))
}

.conceptCompleteTable <- function(result, levels, variable) {
  if (!identical(result$validity.message, "Data in all studies were valid")) {
    stop("Every retained site must release a valid ds.table; pooled table refused.", call. = FALSE)
  }
  counts <- result$output.list$TABLES.COMBINED_all.sources_counts
  observed <- dimnames(counts)[[1L]]
  if (!is.numeric(counts) || length(dim(counts)) != 1L ||
      length(observed) != length(counts) || anyNA(observed) ||
      anyDuplicated(observed) || any(!observed %in% levels) ||
      anyNA(counts) || any(!is.finite(counts) | counts < 0)) {
    stop("ds.table counts do not match the permitted concept contract; rerun publication.", call. = FALSE)
  }
  complete <- array(0, dim = length(levels), dimnames = setNames(list(levels), variable))
  complete[match(observed, levels)] <- as.numeric(counts)
  class(complete) <- "table"
  complete
}
