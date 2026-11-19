#' Tables of published categories, as \code{ds.table()} returns them
#'
#' Calls \code{dsBaseClient::ds.table()} on published dsOMOP factor columns
#' (concepts, categories, calendars, number bins) through a temporary copy whose
#' labels are plain tokens, and returns \code{ds.table()}'s own object with the
#' exact labels back: per-site counts and proportions, combined counts and
#' proportions, the validity message and, when asked, the chi-squared tests.
#' Labels with spaces, commas or any other characters work; declared categories
#' that no site has are shown with zero counts, in the shared level order. The
#' temporary copy is removed afterwards, including on failure; the published
#' table is unchanged.
#'
#' DataSHIELD's rules apply as in \code{ds.table()}: a site whose table has a
#' non-empty cell below \code{nfilter.tab}, or more categories in use than the
#' level rule allows, is reported in the validity message and left out (never
#' filled with zeros). With \code{small = "missing"}, categories (or, with
#' \code{by}, cells) held by fewer than \code{nfilter.tab} people at a site are
#' made missing in the temporary copy first, with secondary suppression so that
#' a total minus the table never gives a small count; a zero can then mean
#' "fewer than nfilter.tab" at that site.
#'
#' Factor columns without a published contract, such as the copies made by
#' \code{\link{ds.omop.drop_empty_levels}} or
#' \code{\link{ds.omop.drop_small_categories}}, are passed to
#' \code{ds.table()} as they are.
#'
#' @param data Character; a published table on every site.
#' @param column Character; a published factor column of \code{data} (the
#'   \code{rvar} of \code{ds.table()}).
#' @param by Optional character; a second published factor column for a
#'   two-way table (the \code{cvar} of \code{ds.table()}).
#' @param useNA,report.chisq.tests As in \code{ds.table()}.
#' @param small \code{"refuse"} (default) leaves DataSHIELD's small-cell
#'   refusal as it is; \code{"missing"} suppresses small categories or cells in
#'   the temporary copy.
#' @param conns Deprecated name of \code{datasources}.
#' @param datasources DSI connections for the retained sites, as in
#'   dsBaseClient. \code{NULL} uses the OMOP session's connections, or every
#'   DataSHIELD connection when there is no session.
#' @return \code{ds.table()}'s object (\code{output.list} with
#'   \code{TABLE_rvar.by.study_counts}, \code{TABLES.COMBINED_all.sources_counts}
#'   and their proportions for one column; per-site and combined counts and
#'   proportions for two; \code{validity.message}), of class
#'   \code{dsomop_table} for printing.
#' @examples
#' \dontrun{
#' tab <- ds.omop.table("D", "cause_of_death_first_value")
#' tab$output.list$TABLES.COMBINED_all.sources_counts
#' tab$output.list$TABLE_rvar.by.study_counts
#' ds.omop.table("D", "cause_of_death_first_value", by = "sex", small = "missing")
#' }
#' @export
ds.omop.table <- function(data, column, conns = NULL, by = NULL, useNA = c("no", "ifany", "always"),
                          report.chisq.tests = FALSE, small = c("refuse", "missing"),
                          datasources = NULL) {
  useNA <- match.arg(useNA)
  small <- match.arg(small)
  for (x in c(list(data, column), if (!is.null(by)) list(by))) {
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !isTRUE(.conceptValidNames(x))) {
      stop("data, column and by must be single valid R names.", call. = FALSE)
    }
  }
  if (identical(by, column)) stop("by must be another column.", call. = FALSE)
  if (!requireNamespace("dsBaseClient", quietly = TRUE)) {
    stop("ds.omop.table requires dsBaseClient.", call. = FALSE)
  }
  conns <- .connections(datasources, conns, fn = "ds.omop.table")
  if (!length(conns) || is.null(names(conns)) || anyDuplicated(names(conns))) {
    stop("Supply named connections for the retained sites.", call. = FALSE)
  }
  reports <- DSI::datashield.aggregate(conns,
    call("omopFactorLevelsDS", as.symbol(data)))
  if (!setequal(names(reports), names(conns)) ||
      any(vapply(reports, is.null, logical(1)))) {
    stop("Incomplete concept level reports; table refused.", call. = FALSE)
  }
  spec <- .conceptUnionReports(reports[names(conns)])
  columns <- c(column, by)
  # Copies for models and tables (drop_empty_levels, drop_small_categories)
  # leave the presentation contract: ds.table() takes their factors as they are.
  mentioned <- unique(unlist(lapply(reports, function(r) c(names(r$levels), r$unsafe)), use.names = FALSE))
  contracted <- vapply(columns, function(col) col %in% c(mentioned, unlist(spec$names), names(spec$columns)), logical(1))
  if (!any(contracted)) {
    if (identical(small, "missing")) {
      stop("small = \"missing\" works on published concept columns; '", data, "' has no contract for them ",
           "(the copies of ds.omop.drop_small_categories() already have small categories missing).", call. = FALSE)
    }
    result <- dsBaseClient::ds.table(rvar = paste0(data, "$", column),
                                     cvar = if (!is.null(by)) paste0(data, "$", by),
                                     useNA = useNA, report.chisq.tests = report.chisq.tests,
                                     datasources = conns)
    attr(result, "variables") <- columns
    class(result) <- c("dsomop_table", "list")
    return(result)
  }
  key_of <- function(col) {
    key <- col
    if (length(spec$names)) key <- names(spec$names)[unlist(spec$names) == col]
    if (length(key) != 1L || is.null(spec$columns[[key]])) {
      stop("Column '", col, "' must have a published concept level contract.", call. = FALSE)
    }
    key
  }
  keys <- vapply(columns, key_of, "")
  # The labels the published columns carry (dsBase-safe ones when published so).
  levels <- lapply(keys, function(key) {
    column <- spec$columns[[key]]
    safe <- any(vapply(reports, function(r) key %in% unlist(r$safe_columns), logical(1)))
    as.character(unlist((if (safe) .conceptSafeColumn(column) else column)$levels, use.names = FALSE))
  })
  # Reserve fresh symbols on every site; never overwrite the publication.
  inventory <- unlist(.plan_symbol_inventory(conns, "concept table analysis"), use.names = FALSE)
  fresh <- function() {
    for (attempt in seq_len(10L)) {
      candidate <- .generate_symbol("dsOtable")
      if (candidate != data && !candidate %in% inventory) return(candidate)
    }
    stop("Could not reserve a table analysis symbol.", call. = FALSE)
  }
  temporary <- fresh()
  on.exit({
    for (site in names(conns)) {
      tryCatch(DSI::datashield.rm(conns[site], temporary),
        error = function(e) warning("Could not remove table analysis copy on ", site,
                                    ": ", conditionMessage(e), call. = FALSE))
    }
  }, add = TRUE)
  assign_all <- function(expr, what) {
    succeeded <- character()
    failed <- character()
    DSI::datashield.assign.expr(conns, temporary, expr,
      success = function(server) succeeded <<- c(succeeded, server),
      error = function(server, message) failed[[server]] <<- message)
    if (length(failed) || !setequal(succeeded, names(conns))) {
      stop(what, " failed; matching servers are required. ", paste(failed, collapse = "; "), call. = FALSE)
    }
  }
  # The appended formal also makes older servers refuse instead of silently
  # ignoring the request and passing public labels to the lossy dsBase codec.
  table_column <- if (length(keys) == 1L) unname(keys) else .ds_encode(unname(keys))
  assign_all(call("omopAsFactorColumnsDS", as.symbol(data), .ds_encode(spec), table_column),
             "Concept table analysis copy")
  if (identical(small, "missing")) {
    assign_all(call("omopAsFactorColumnsDS", as.symbol(temporary),
                    .ds_encode(list(kind = "suppress_small", columns = as.list(columns), joint = TRUE))),
               "Small-category suppression")
  }
  # ds.table() stops, rather than reporting, when a site has no value left
  # (for example every cell suppressed); answer as its other refusals do.
  result <- tryCatch(dsBaseClient::ds.table(rvar = paste0(temporary, "$", column),
                                  cvar = if (!is.null(by)) paste0(temporary, "$", by),
                                  useNA = useNA, report.chisq.tests = report.chisq.tests,
                                  datasources = conns),
    error = function(e) list(validity.message = paste0("ds.table() could not tabulate at every site",
      if (identical(small, "missing")) " (a site may have no category left once small ones are missing)", ": ",
      conditionMessage(e)), error.messages = conditionMessage(e)))
  out <- .conceptTableComplete(result, setNames(levels, c("C", "D")[seq_along(levels)]),
                               rename = setNames(paste0(data, "$"), paste0(temporary, "$")))
  attr(out, "variables") <- columns
  if (identical(small, "missing")) attr(out, "small") <- "missing"
  class(out) <- c("dsomop_table", "list")
  out
}

# ds.table()'s object with the tokens of the temporary copy (C1.. for the first
# column, D1.. for the second) back to labels, and the declared categories that
# no valid site has added: zero counts; for proportions, zero along the other
# margin and NA where the added category is the one normalised. Dimensions are
# known by position (first column, second column, sites), never by their names.
.conceptTableComplete <- function(result, maps, rename = NULL) {
  if (!is.list(result) || is.null(result$output.list)) return(result)
  bad <- function() stop("ds.table counts do not match the permitted concept contract; rerun publication.", call. = FALSE)
  two_way <- length(maps) == 2L
  complete <- function(x, name) {
    if (!(is.array(x) || is.table(x)) || is.null(dimnames(x))) return(x)
    kind <- if (grepl("counts", name, fixed = TRUE)) "counts" else
      if (grepl("row.props", name, fixed = TRUE)) "row" else "col"
    values <- unclass(x)
    if (identical(kind, "counts") && (anyNA(values) || any(!is.finite(values)) || any(values < 0))) bad()
    # One-way tables by site have the sites second; two-way tables, the second column.
    for (d in if (two_way && !grepl("by.study", name, fixed = TRUE)) seq_len(min(2L, length(dim(x)))) else 1L) {
      prefix <- names(maps)[[d]]
      tokens <- paste0(prefix, seq_along(maps[[prefix]]))
      current <- dimnames(x)[[d]]
      missing_level <- is.na(current) | current == "NA"
      if (!all(current[!missing_level] %in% tokens)) bad()
      fill <- if (identical(kind, "counts")) 0 else
        if ((identical(kind, "row") && d == 1L) || (identical(kind, "col") && d == 2L)) NA_real_ else 0
      x <- .tableExpand(x, d, c(tokens, current[missing_level]), fill)
      labelled <- dimnames(x)
      labelled[[d]] <- c(maps[[prefix]], current[missing_level])
      dimnames(x) <- labelled
    }
    # Dimensions are named after the published table, not the temporary copy.
    if (length(rename) && !is.null(names(dimnames(x)))) {
      for (from in names(rename)) names(dimnames(x)) <- sub(from, rename[[from]], names(dimnames(x)), fixed = TRUE)
    }
    x
  }
  result$output.list <- Map(complete, result$output.list, names(result$output.list))
  result
}

# x with dimension d laid out as `target` (existing entries kept, new ones `fill`).
.tableExpand <- function(x, d, target, fill) {
  dims <- dim(x)
  names_x <- dimnames(x)
  new_names <- names_x
  new_names[[d]] <- target
  dims[[d]] <- length(target)
  out <- array(fill, dim = dims, dimnames = new_names)
  index <- rep(list(TRUE), length(dims))
  index[[d]] <- match(names_x[[d]], target)
  out <- do.call(`[<-`, c(list(out), index, list(value = unclass(x))))
  if (is.table(x)) class(out) <- class(x)
  out
}

#' The combined counts of a ds.omop.table() result, as the plain table that
#' dsOMOPClient 2.10.8 and earlier returned.
#' @param x A \code{dsomop_table}.
#' @param ... Unused.
#' @export
as.table.dsomop_table <- function(x, ...) {
  counts <- x$output.list$TABLES.COMBINED_all.sources_counts
  if (is.null(counts)) stop(paste(x$validity.message, collapse = " "), call. = FALSE)
  if (!is.table(counts)) counts <- as.table(counts)
  counts
}

#' @export
print.dsomop_table <- function(x, ...) {
  variables <- attr(x, "variables")
  ol <- x$output.list
  if (is.null(ol)) {
    cat(paste(x$validity.message, collapse = "\n"), "\n")
    # As ds.table() reports them: each site's reason.
    reasons <- x$error.messages
    if (is.list(reasons) && length(reasons)) {
      for (n in names(reasons)) cat(n, ": ", paste(unlist(reasons[[n]]), collapse = " "), "\n", sep = "")
    } else if (length(reasons) && !identical(reasons, x$validity.message)) {
      cat(paste(reasons, collapse = "\n"), "\n")
    }
    return(invisible(x))
  }
  cat("Combined (all valid sites):", paste(variables, collapse = " by "), "\n")
  print(ol$TABLES.COMBINED_all.sources_counts)
  by_site <- if (length(variables) == 1L) ol$TABLE_rvar.by.study_counts else
    ol[grepl("^TABLE[._]STUDY.*_counts$", names(ol))]
  if (!is.null(by_site) && length(by_site)) {
    cat("\nBy site:\n")
    if (is.list(by_site) && !is.array(by_site)) for (n in names(by_site)) { cat(n, "\n"); print(by_site[[n]]) } else print(by_site)
  }
  if (identical(attr(x, "small"), "missing")) {
    cat("\nCategories held by fewer than nfilter.tab people at a site are missing at that site.\n")
  }
  if (!identical(x$validity.message, "Data in all studies were valid")) {
    cat("\n", paste(x$validity.message, collapse = "\n"), "\n", sep = "")
  }
  invisible(x)
}
