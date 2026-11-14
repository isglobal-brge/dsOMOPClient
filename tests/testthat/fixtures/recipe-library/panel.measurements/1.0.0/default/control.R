# Literal control transcribed from the endorsed section-4 expansion.
# Synthetic binding constants are study fixtures, not catalogue defaults.
events <- list()
intervals <- list()
variables <- list()
index <- omop_event("visit_occurrence", 9201, name = "entry")
select <- "first_eligible"
populations <- list(omop_population("study",
  index = omop_index(index, select = select,
                     end = omop_end("observation_end")),
  coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                           on_missing = "drop", gap = 0L)))

measure <- omop_event("measurement", 990003, value = "value_as_number", unit = omop_unit(8840), name = "measure")
horizon <- 365L
bin_days <- 30L
lookback <- 90L
expire <- 30L
outputs <- list(omop_output("measurements", rows = "study", format = "person_period",
  grain = "episode", options = list(representation = "dense", sources = list(measure),
    bins = list(width = bin_days), window = omop_window(0L, horizon),
    analyses = c("count", "mean", "last"),
    carry_forward = list(measure = list(lookback = lookback, expire = expire)))))
publish <- "measurements"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
