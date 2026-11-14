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

outcome_a <- omop_event("condition_occurrence", 990005, name = "cause_a")
outcome_b <- omop_event("condition_occurrence", 990008, name = "cause_b")
horizon <- 365L
outputs <- list(omop_output("competing_risk", rows = "study", format = "survival",
  grain = "episode", options = list(outcomes = list(cause_a = outcome_a, cause_b = outcome_b),
    tar = omop_window(1L, horizon), format = "competing_risk", event_order = "first",
    censor = list("observation_end", "death"),
    prior_outcome = list(lookback = 365L, action = "exclude"),
    tie_policy = "priority", same_day_priority = "event")))
publish <- "competing_risk"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
