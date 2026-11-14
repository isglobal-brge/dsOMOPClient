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

source_a <- omop_event("measurement", 990003, value = "value_as_number", name = "measure")
source_b <- omop_event("condition_occurrence", 990004, name = "history")
outputs <- list(omop_output("event_log", rows = "study", format = "long",
  grain = "episode", sources = list(source_a, source_b),
  options = list(event_log = TRUE, unit = "episode_event", date_handling = "relative")))
publish <- "event_log"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
