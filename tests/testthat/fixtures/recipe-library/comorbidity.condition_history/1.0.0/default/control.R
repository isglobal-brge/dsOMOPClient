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

conditions <- 990004L
lookback <- 365L
dx <- omop_event("condition_occurrence", conditions, name = "history")
events <- list(dx)
variables <- list(
  history_present = omop_variable(event = dx,
    reducer = omop_reduce("binary", window = omop_window(-lookback, -1L))),
  history_days = omop_variable(event = dx,
    reducer = omop_reduce("count", distinct = "day",
                          window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("history", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "history"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
