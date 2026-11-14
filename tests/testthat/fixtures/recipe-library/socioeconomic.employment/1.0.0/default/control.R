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

employment <- 990002L
lookback <- 365L
job <- omop_event("observation", employment,
                  name = "employment")
events <- list(job)
variables <- list(
  employment_concept_id = omop_variable(event = job,
    reducer = omop_reduce("last_value", value = "value_as_concept_id", window = omop_window(-lookback, -1L),
                          missing = "keep")),
  employment_records = omop_variable(event = job,
    reducer = omop_reduce("count", window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("employment", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "employment"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
