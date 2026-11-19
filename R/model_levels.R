#' Drop levels that are empty at every site, for models
#'
#' Published factor columns share one level set across sites, including levels
#' that no site uses (declared vocabulary lists, calendars, level ranges). A
#' level empty at every site gives an all-zero design column, so a pooled
#' \code{ds.glm()} fails as singular. This makes a copy for models whose factor
#' columns keep only the levels that hold a value at some site, identical at
#' every site and in the same order; each copy keeps its own rows and values.
#'
#' Which levels hold a value at a site is an observed-level release: each site
#' reports it under DataSHIELD's factor-level rule (\code{nfilter.levels.max}
#' and \code{nfilter.levels.density}, as \code{ds.asFactor} does), without
#' counts. A column whose levels in use fail that rule at some site keeps all
#' its levels; group it first (\code{omop_category_recode()},
#' \code{omop_category_rollup()}), which also suits models better.
#'
#' The copy is for analysis. Its dropped columns no longer carry the dsOMOP
#' presentation contract, so publish again from the recipe for anything else.
#'
#' @param x Character; a published in-memory table on every site (a
#'   \code{recipe_execute()} output, for example).
#' @param columns Optional character vector of factor columns; by default every
#'   published concept, category or calendar column (the columns the servers
#'   reconciled; other factors, such as legacy age bands, are left as they are).
#' @param newobj Character; the server-side name of the copy.
#' @param symbol Character; the OMOP session symbol.
#' @param conns Deprecated name of \code{datasources}.
#' @param datasources DSI connections, as in dsBaseClient; defaults to the
#'   session's connections.
#' @return Invisibly, a data frame with one row per column: \code{levels}
#'   before, \code{kept} and \code{status}.
#' @examples
#' \dontrun{
#' recipe_execute(recipe, out = c(study = "D"))
#' ds.omop.drop_empty_levels("D", newobj = "D_model")
#' dsBaseClient::ds.glm(died ~ cause_of_death, data = "D_model", family = "binomial")
#' }
#' @export
ds.omop.drop_empty_levels <- function(x, columns = NULL, newobj = paste0(x, "_model"),
                                      symbol = "omop", conns = NULL, datasources = NULL) {
  session <- .get_session(symbol)
  conns <- .connections(datasources, conns, symbol, session, "ds.omop.drop_empty_levels")
  .model_levels_assert_servers(session, names(conns))
  x <- .resolve_target_symbol(x, session, "x")
  if (!is.null(columns) && (!is.character(columns) || !length(columns) || anyNA(columns) ||
      anyDuplicated(columns))) {
    stop("ds.omop.drop_empty_levels(): columns must be distinct column names.", call. = FALSE)
  }
  raw <- .ds_safe_aggregate(conns, expr = call("omopFactorLevelsDS", as.name(x),
    .ds_encode(list(columns = as.list(columns %||% "*")))))
  failed <- union(names(attr(raw, "ds_errors")), setdiff(names(conns), names(raw)))
  if (length(failed)) {
    stop("ds.omop.drop_empty_levels(): the levels in use could not be read on ",
         paste(failed, collapse = ", "), ". ",
         paste(unlist(attr(raw, "ds_errors"), use.names = FALSE), collapse = "; "), call. = FALSE)
  }
  reports <- raw[names(conns)]
  # A site reports a column either with its level set or as refused.
  cols <- unique(unlist(lapply(reports, function(r) c(names(r$levels), r$unsafe)), use.names = FALSE))
  for (site in names(reports)) {
    if (!setequal(c(names(reports[[site]]$levels), reports[[site]]$unsafe), cols)) {
      stop("ds.omop.drop_empty_levels(): '", x, "' does not publish the same factor columns at every site; ",
           "publish it with recipe_execute() first.", call. = FALSE)
    }
  }
  shared <- list()
  for (col in cols) {
    sets <- Filter(Negate(is.null), lapply(reports, function(r) r$levels[[col]]))
    if (length(sets) && any(!vapply(sets, function(s) identical(as.character(unlist(s)),
        as.character(unlist(sets[[1L]]))), logical(1)))) {
      stop("ds.omop.drop_empty_levels(): '", x, "' has different factor levels across sites; ",
           "publish it with recipe_execute() first.", call. = FALSE)
    }
    shared[[col]] <- if (length(sets)) as.character(unlist(sets[[1L]], use.names = FALSE)) else character()
  }
  kept <- list()
  summary <- data.frame(column = cols, levels = integer(length(cols)), kept = integer(length(cols)),
                        status = character(length(cols)), stringsAsFactors = FALSE)
  for (i in seq_along(cols)) {
    col <- cols[[i]]
    levels <- shared[[col]]
    summary$levels[[i]] <- summary$kept[[i]] <- if (length(levels)) length(levels) else NA_integer_
    blocked <- names(reports)[vapply(reports, function(r) col %in% r$unsafe, logical(1))]
    if (length(blocked)) {
      summary$status[[i]] <- paste0("kept: its levels in use fail the DataSHIELD level rule on ",
                                    paste(blocked, collapse = ", "))
      next
    }
    used <- unique(unlist(lapply(reports, function(r) r$occurring[[col]]), use.names = FALSE))
    keep <- levels[levels %in% used]
    if (!length(keep)) {
      summary$status[[i]] <- "kept: no site holds a value"
      next
    }
    if (length(keep) == length(levels)) {
      summary$status[[i]] <- "no empty levels"
      next
    }
    kept[[col]] <- as.list(keep)
    summary$kept[[i]] <- length(keep)
    summary$status[[i]] <- "dropped levels empty at every site"
    reference <- unlist(Filter(Negate(is.null), lapply(reports, function(r) r$reference[[col]]))[1L], use.names = FALSE)
    if (length(reference) && !is.na(reference[[1L]]) && !reference[[1L]] %in% keep) {
      summary$status[[i]] <- paste0(summary$status[[i]], "; the reference '", reference[[1L]],
                                    "' was one of them, so the first kept category is the reference")
    }
  }
  .assign_expr_atomic(conns, newobj,
    expr = call("omopAsFactorColumnsDS", as.name(x),
                .ds_encode(list(kind = "drop_levels", columns = kept))),
    context = "Model levels", session_symbol = symbol, required_symbols = x)
  blocked <- summary$column[startsWith(summary$status, "kept: its levels")]
  if (length(blocked)) {
    message("ds.omop.drop_empty_levels(): ", paste(blocked, collapse = ", "),
            " keep all their levels; group them (omop_category_recode() / omop_category_rollup()) for models.")
  }
  invisible(summary)
}

# Level reports and model copies need dsOMOP 2.10.7 on every site.
.model_levels_assert_servers <- function(session, servers, needed = "2.10.7",
                                         what = "ds.omop.drop_empty_levels()") {
  versions <- session$server_versions %||% character()
  old <- servers[!vapply(servers, function(site) {
    version <- if (site %in% names(versions)) versions[[site]] else NULL
    is.character(version) && length(version) == 1L && !is.na(version) &&
      grepl("^[0-9]+([.][0-9]+)+$", version) &&
      tryCatch(utils::compareVersion(version, needed) >= 0L, error = function(e) FALSE)
  }, logical(1))]
  if (length(old)) {
    stop(what, " needs dsOMOP ", needed, " or later on: ",
         paste(old, collapse = ", "), ". Upgrade the server and publish its methods.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Make categories held by very few people missing, for tables
#'
#' DataSHIELD refuses a table or a summary of a factor at any site where a
#' non-empty category has fewer than \code{nfilter.tab} people (dsBase's
#' \code{ds.summary()} answers "INVALID object!"); those counts are never
#' released. This makes a copy where, at each site, the values of such
#' categories are missing, so that tables and summaries of the other
#' categories work. While the people made missing number between 1 and
#' \code{nfilter.tab - 1}, the next smallest category is made missing too, so
#' that a total minus a table never gives a small count. Levels that no site
#' uses are then dropped, as \code{ds.omop.drop_empty_levels()} does.
#'
#' The copy loses those people's values in these columns: use it for
#' descriptive tables and summaries. For models, group rare categories instead
#' (\code{omop_category_recode()}, \code{omop_category_rollup()}). Each column is
#' treated on its own, so a two-way table of the copy can still have small
#' cells; \code{ds.omop.table(by = , small = "missing")} handles those.
#'
#' @inheritParams ds.omop.drop_empty_levels
#' @param newobj Character; the server-side name of the copy.
#' @return Invisibly, the summary of \code{ds.omop.drop_empty_levels()} for the
#'   copy.
#' @examples
#' \dontrun{
#' ds.omop.drop_small_categories("D", newobj = "D_small")
#' dsBaseClient::ds.summary("D_small$cause_of_death_first_value")
#' ds.omop.table("D_small", "cause_of_death_first_value")
#' }
#' @export
ds.omop.drop_small_categories <- function(x, columns = NULL, newobj = paste0(x, "_small"),
                                          symbol = "omop", conns = NULL, datasources = NULL) {
  session <- .get_session(symbol)
  conns <- .connections(datasources, conns, symbol, session, "ds.omop.drop_small_categories")
  .model_levels_assert_servers(session, names(conns), "2.10.9", "ds.omop.drop_small_categories()")
  x <- .resolve_target_symbol(x, session, "x")
  if (!is.null(columns) && (!is.character(columns) || !length(columns) || anyNA(columns) ||
      anyDuplicated(columns))) {
    stop("ds.omop.drop_small_categories(): columns must be distinct column names.", call. = FALSE)
  }
  inventory <- unlist(.plan_symbol_inventory(conns, "Small categories"), use.names = FALSE)
  staging <- NULL
  for (attempt in seq_len(10L)) {
    candidate <- .generate_symbol("dsOsmall")
    if (!candidate %in% c(inventory, x, newobj)) { staging <- candidate; break }
  }
  if (is.null(staging)) stop("Could not reserve a symbol for the small-category copy.", call. = FALSE)
  on.exit(for (site in names(conns)) tryCatch(DSI::datashield.rm(conns[site], staging),
    error = function(e) NULL), add = TRUE)
  .assign_expr_atomic(conns, staging,
    expr = call("omopAsFactorColumnsDS", as.name(x),
                .ds_encode(list(kind = "suppress_small", columns = as.list(columns %||% character()), joint = FALSE))),
    context = "Small categories", session_symbol = symbol, required_symbols = x)
  invisible(ds.omop.drop_empty_levels(staging, columns = columns, newobj = newobj, symbol = symbol,
                                      datasources = conns))
}
