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

education <- 990001L
lookback <- 365L
edu <- omop_event("observation", education,
                  name = "education")
events <- list(edu)
variables <- list(
  education_concept_id = omop_variable(event = edu,
    reducer = omop_reduce("last_value", value = "value_as_concept_id", window = omop_window(-lookback, -1L),
                          missing = "keep")),
  education_records = omop_variable(event = edu,
    reducer = omop_reduce("count", window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("education", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "education"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
