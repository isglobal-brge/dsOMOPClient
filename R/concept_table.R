#' Pool a published concept category with its complete level contract
#'
#' Calls the real \code{dsBaseClient::ds.table} on a published in-memory
#' concept factor through a temporary protected copy with lossless category
#' tokens. Uses the existing disclosure-checked factor harmonisation transport
#' and maps counts back to the exact public labels, restoring unused declared
#' categories as zero cells in federation level order. All sites
#' must return valid counts; failed or suppressed tables are never zero-filled.
#'
#' dsBase rebuilds factors from observed values, so a direct \code{ds.table}
#' call can omit globally unused levels. Use this integration when the pooled
#' table must retain the complete declared category contract. Counts exclude
#' actual missing values; a literal label \code{"NA"} remains a category.
#' Commas and other free text are preserved. The temporary copy is removed
#' after analysis, including on failure; the published object is unchanged.
#' Requires the matching server implementation of the table analysis transport.
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
  # Reserve a fresh symbol on every site; never overwrite the publication.
  inventory <- .plan_symbol_inventory(conns, "concept table analysis")
  temporary <- NULL
  for (attempt in seq_len(10L)) {
    candidate <- .generate_symbol("dsOtable")
    if (candidate != data && !candidate %in% unlist(inventory, use.names = FALSE)) {
      temporary <- candidate
      break
    }
  }
  if (is.null(temporary)) stop("Could not reserve a table analysis symbol.", call. = FALSE)
  on.exit({
    for (site in names(conns)) {
      tryCatch(DSI::datashield.rm(conns[site], temporary),
        error = function(e) warning("Could not remove table analysis copy on ", site,
                                    ": ", conditionMessage(e), call. = FALSE))
    }
  }, add = TRUE)
  succeeded <- character()
  failed <- character()
  DSI::datashield.assign.expr(conns, temporary,
    # The appended formal also makes older servers refuse instead of silently
    # ignoring the request and passing public labels to the lossy dsBase codec.
    call("omopAsFactorColumnsDS", as.symbol(data), .ds_encode(spec), key),
    success = function(server) succeeded <<- c(succeeded, server),
    error = function(server, message) failed[[server]] <<- message)
  if (length(failed) || !setequal(succeeded, names(conns))) {
    stop("Concept table analysis copy failed; matching servers are required. ",
         paste(failed, collapse = "; "), call. = FALSE)
  }
  tokens <- paste0("C", seq_along(levels))
  result <- dsBaseClient::ds.table(rvar = paste0(temporary, "$", column),
                                  useNA = "no", datasources = conns)
  complete <- .conceptCompleteTable(result, tokens, paste0(data, "$", column))
  dimnames(complete)[[1L]] <- levels
  complete
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
