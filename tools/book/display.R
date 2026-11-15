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
