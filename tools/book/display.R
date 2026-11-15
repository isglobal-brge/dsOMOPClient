# Presentation uses only released aggregates. No helper fetches private rows.
.book_released_counts <- function(result) {
  if (!identical(result$validity.message, "Data in all studies were valid"))
    stop("Count table was withheld or incomplete; inspect the original result.")
  counts <- result$output.list$TABLES.COMBINED_all.sources_counts
  if (is.null(counts) || !is.numeric(counts) || anyNA(counts))
    stop("No released numeric counts in this result.")
  counts
}

book_counts <- function(result) {
  counts <- .book_released_counts(result)
  if (length(dim(counts)) > 1L) stop("Use book_table2() for a two-way table.")
  data.frame(Category = names(counts), Count = as.numeric(counts), check.names = FALSE)
}

# A joint release retains the sites' common category labels and zero cells.
book_site_counts <- function(result) {
  .book_released_counts(result)
  counts <- result$output.list$TABLE_rvar.by.study_counts
  if (!is.matrix(counts) || !is.numeric(counts) || anyNA(counts) ||
      is.null(rownames(counts)) || is.null(colnames(counts)))
    stop("No complete released per-site counts in this result.")
  counts
}

# The ds.table two-way result includes many proportions; show just its counts.
book_table2 <- function(result) {
  counts <- .book_released_counts(result)
  if (length(dim(counts)) != 2L) stop("Expected a two-way ds.table() result.")
  matrix(as.numeric(counts), nrow = nrow(counts), dimnames = dimnames(counts))
}

# Prefer book_summary("D_people$age", datasources = conns). For a saved
# ds.summary() result, also supply missing = ds.numNA(...): ds.summary itself
# reports total length but does NOT report how many values are missing.
book_summary <- function(result, missing = NULL, datasources = NULL) {
  if (is.character(result) && length(result) == 1L) {
    column <- result
    result <- dsBaseClient::ds.summary(column, datasources = datasources)
    missing <- dsBaseClient::ds.numNA(column, datasources = datasources)
  }
  if (is.null(missing))
    stop("Supply missing = dsBaseClient::ds.numNA(...) or pass a column name and datasources.")
  rows <- lapply(names(result), function(site) {
    values <- result[[site]][["quantiles & mean"]]
    total <- result[[site]][["length"]]
    absent <- missing[[site]]
    if (!is.numeric(values) || !length(values) || anyNA(values) ||
        !is.numeric(total) || length(total) != 1L || !is.finite(total) ||
        !is.numeric(absent) || length(absent) != 1L || !is.finite(absent) ||
        absent < 0 || absent > total)
      stop("Numeric summary or missing count was withheld or incomplete; inspect the original results.")
    data.frame(Site = site, Statistic = names(values), Value = as.numeric(values),
      `N valid` = total - absent, `N missing` = absent, check.names = FALSE)
  })
  if (!length(rows)) stop("No released numeric summary.")
  do.call(rbind, rows)
}

# Counts describe the groups; the numeric mean describes the whole population.
# Keep it on ONE clearly labelled row, never on each category row.
book_group_comparison <- function(counts, mean_result, number, unit) {
  out <- book_counts(counts)
  values <- mean_result$Global.Mean
  if (!is.matrix(values) || !is.finite(values[1L, "EstimatedMean"]))
    stop("Numeric mean was withheld or incomplete; inspect the original result.")
  out$Number <- number
  out$Unit <- unit
  out$`Overall mean` <- NA_real_
  rbind(out, data.frame(Category = "Overall (all groups)", Count = NA_real_,
    Number = number, Unit = unit, `Overall mean` = as.numeric(values[1L, "EstimatedMean"]),
    check.names = FALSE))
}

# Accept either ds.omop.columns(...) or one site's returned data frame.
# rows optionally selects column names; columns selects metadata to teach.
book_columns <- function(result,
                         columns = c("column_name", "cdm_datatype", "is_date", "is_blocked"),
                         rows = NULL) {
  select <- function(x) {
    if (!is.data.frame(x) || !all(columns %in% names(x)))
      stop("Expected released column metadata with the selected columns.")
    if (!is.null(rows)) x <- x[x$column_name %in% rows, , drop = FALSE]
    x[, columns, drop = FALSE]
  }
  if (is.data.frame(result)) return(select(result))
  if (!is.list(result) || !length(result) || is.null(names(result)))
    stop("Expected ds.omop.columns() metadata.")
  tables <- lapply(names(result), function(site) {
    x <- select(result[[site]])
    if (length(result) > 1L) x <- cbind(Site = rep(site, nrow(x)), x)
    x
  })
  do.call(rbind, tables)
}

# One compact line per component, without receipt fields or attached codebooks.
# Identical mappings from several sites appear only once.
book_symbols <- function(result) {
  lines <- character()
  walk <- function(x, path = character()) {
    if (is.list(x) && is.character(x$symbol) && length(x$symbol) == 1L) {
      label <- paste(tail(path, 2L), collapse = "/")
      lines <<- c(lines, paste(label, "\u2192", x$symbol))
    } else if (is.character(x)) {
      for (i in seq_along(x)) {
        label <- paste(c(path, names(x)[i]), collapse = "/")
        lines <<- c(lines, paste(label, "\u2192", x[[i]]))
      }
    } else if (is.list(x) && !is.null(names(x))) {
      for (name in names(x)) walk(x[[name]], c(path, name))
    }
  }
  if (is.null(names(result))) stop("Expected a named output-to-symbol mapping.")
  walk(result)
  if (!length(lines)) stop("No published symbols in this result.")
  cat(paste(unique(lines), collapse = "\n"), "\n", sep = "")
  invisible(result)
}

# In a knitted report that sources this file, such as this book, a data frame or
# matrix left at the end of a step prints as a table, without row numbers. The
# chunk options table.digits and table.row.names = FALSE round its numbers and
# leave out its row names. The console print is unchanged.
if (isNamespaceLoaded("knitr")) local({
  # Text such as $.path, x^2 or --1st is shown as written, never read as Markdown.
  literal <- function(text) {
    text <- gsub("([\\\\`*$^~<>|@])", "\\\\\\1", text, perl = TRUE)
    text <- gsub("((?<=-)-|-(?=-)|(?<=\\.)\\.|\\.(?=\\.))", "\\\\\\1", text, perl = TRUE)
    gsub("(?<![[:alnum:]])_|_(?![[:alnum:]])", "\\\\_", text, perl = TRUE)
  }
  print_table <- function(x, options = list(), ...) {
    # Other classes, and one-way counts, keep their usual print.
    if (is.data.frame(x) && !identical(class(x), "data.frame") || length(dim(x)) != 2L)
      return(knitr::normal_print(x))
    no_names <- is.data.frame(x) && all(grepl("^[0-9]+$", rownames(x))) ||
      isFALSE(options$table.row.names)
    if (is.data.frame(x)) {
      text <- vapply(x, function(column) is.character(column) || is.factor(column), logical(1))
      x[text] <- lapply(x[text], function(column) literal(as.character(column)))
    } else if (is.character(x)) x[] <- literal(x)
    dimnames(x) <- lapply(dimnames(x), function(names) if (is.null(names)) NULL else literal(names))
    args <- list(x, row.names = if (no_names) FALSE else NA)
    if (!is.null(options$table.digits)) args$digits <- options$table.digits
    knitr::knit_print(do.call(knitr::kable, args), options = options, ...)
  }
  for (type in c("data.frame", "matrix", "table"))
    registerS3method("knit_print", type, print_table, envir = asNamespace("knitr"))
})
