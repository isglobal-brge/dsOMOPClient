.test_safe_bins <- function(breaks = c(0, 5, 10, 15, 20),
                            table = "measurement",
                            column = "value_as_number",
                            concept_id = 3004410L,
                            concept_col = NULL,
                            n_bins = max(2L, length(breaks) - 1L)) {
  list(
    breaks = breaks,
    contract = list(
      table = table,
      column = column,
      concept_id = concept_id,
      concept_col = concept_col,
      n_bins = as.integer(n_bins)
    )
  )
}
