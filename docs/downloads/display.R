# Presentation only: these functions consume already released aggregate objects.
book_counts <- function(result) {
  if (!identical(result$validity.message, "Data in all studies were valid"))
    stop("Count table was withheld or incomplete; inspect the original result.")
  counts <- result$output.list$TABLES.COMBINED_all.sources_counts
  if (is.null(counts) || !is.numeric(counts) || anyNA(counts))
    stop("No released numeric counts in this result.")
  data.frame(Category = names(counts), Count = as.numeric(counts), check.names = FALSE)
}
book_summary <- function(result) {
  rows <- lapply(names(result), function(site) {
    values <- result[[site]][["quantiles & mean"]]
    if (!is.numeric(values) || !length(values) || anyNA(values))
      stop("Numeric summary was withheld or incomplete; inspect the original result.")
    data.frame(Site = site, Statistic = names(values), Value = as.numeric(values))
  })
  if (!length(rows)) stop("No released numeric summary.")
  do.call(rbind, rows)
}
