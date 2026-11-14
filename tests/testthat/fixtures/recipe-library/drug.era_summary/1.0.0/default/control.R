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

drug <- 990006L
gap <- 30L
persistence <- 0L
window <- omop_window(0L, 365L)
rx <- omop_event("drug_exposure", drug, end = list(fallback = "days_supply"),
                 name = "rx")
course <- omop_era(rx, gap = gap, persistence = persistence, min_days = 0L,
                   by = "event", end = "recorded_end", fallback = "days_supply",
                   stockpile = FALSE, name = "course")
events <- list(rx)
intervals <- list(course)
variables <- list(
  era_count = omop_variable(event = course,
    reducer = omop_reduce("era_count", window = window)),
  era_days = omop_variable(event = course,
    reducer = omop_reduce("era_days", window = window)),
  first_era_days = omop_variable(event = course,
    reducer = omop_reduce("first_era_days", window = window)),
  covered_days = omop_variable(event = course,
    reducer = omop_reduce("covered_days", window = window, coverage = "recorded")))
outputs <- list(omop_output("eras", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "eras"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
