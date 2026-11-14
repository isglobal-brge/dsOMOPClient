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

outcome <- omop_event("condition_occurrence", 990005, name = "outcome")
horizon <- 365L
outputs <- list(omop_output("incidence", rows = "study", format = "person_period",
  grain = "episode", options = list(representation = "dense",
    bins = list(calendar = "year"), window = omop_window(0L, horizon),
    outcomes = list(outcome = outcome))))
publish <- "incidence"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
