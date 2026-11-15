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

# Put released category counts beside the corresponding released numeric mean.
book_group_comparison <- function(counts, mean_result, number, unit) {
  out <- book_counts(counts)
  values <- mean_result$Global.Mean
  if (!is.matrix(values) || !is.finite(values[1L, 1L]))
    stop("Numeric mean was withheld or incomplete; inspect the original result.")
  out$Number <- number
  out$Unit <- unit
  out$Mean <- as.numeric(values[1L, 1L])
  out
}

# Display the output-to-symbol mapping without printing the attached codebook.
# Keep the original result when calling recipe_codebook() later.
book_symbols <- function(result) {
  if (!is.character(result) || is.null(names(result)))
    stop("Expected a successful named output-to-symbol mapping.")
  print(setNames(as.character(result), names(result)))
  invisible(result)
}
