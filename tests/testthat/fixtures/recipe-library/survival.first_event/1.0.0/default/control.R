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
start <- 1L
horizon <- 365L
prior_days <- 365L
outputs <- list(omop_output("survival", rows = "study", format = "survival",
  grain = "episode", options = list(outcomes = list(outcome = outcome),
    tar = omop_window(start, horizon), format = "survival", event_order = "first",
    censor = list("observation_end", "death"),
    prior_outcome = list(lookback = prior_days, action = "exclude"),
    tie_policy = "priority", same_day_priority = "event")))
publish <- "survival"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
