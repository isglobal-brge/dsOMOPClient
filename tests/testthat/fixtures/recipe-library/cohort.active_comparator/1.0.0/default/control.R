# Literal control transcribed from the endorsed section-4 expansion.
# Synthetic binding constants are study fixtures, not catalogue defaults.
events <- list()
intervals <- list()
variables <- list()
drug_a <- 990006L
drug_b <- 990007L
outcome <- omop_event("condition_occurrence", 990005, name = "outcome")
washout <- 365L
horizon <- 365L
grace <- 30L
a <- omop_event("drug_exposure", drug_a, name = "rx_a")
b <- omop_event("drug_exposure", drug_b, name = "rx_b")
ea <- omop_era(a, by = "event", gap = 30L, persistence = 0L,
               end = "recorded_end", fallback = "days_supply", name = "era_a")
eb <- omop_era(b, by = "event", gap = 30L, persistence = 0L,
               end = "recorded_end", fallback = "days_supply", name = "era_b")
events <- list(a, b)
intervals <- list(ea, eb)
populations <- list(omop_population("study",
  index = omop_index(omop_any_of(ea, eb, order = c("era_a", "era_b")),
    select = "first_eligible", end = omop_end("interval_end")),
  coverage = omop_coverage(), filters = list(
    omop_observed(before = washout),
    omop_has_not(a, window = omop_window(-washout, -1L)),
    omop_has_not(b, window = omop_window(-washout, -1L)))))
variables <- list(arm = omop_population_column("index_source"),
                   age = omop_age(), sex = omop_sex())
outputs <- list(
  omop_output("baseline", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")),
  omop_output("itt", rows = "study", format = "survival", grain = "episode",
    options = list(outcomes = list(outcome = outcome),
      tar = omop_window(1L, horizon), format = "survival", event_order = "first",
      prior_outcome = list(lookback = 365L, action = "exclude"),
      censor = list("observation_end", "death"), same_day_priority = "event")),
  omop_output("on_treatment", rows = "study", format = "survival", grain = "episode",
    options = list(outcomes = list(outcome = outcome),
      tar = omop_window(1L, horizon), format = "survival", event_order = "first",
      prior_outcome = list(lookback = 365L, action = "exclude"),
      censor = list("observation_end", "death",
        omop_censor(ea, which = "containing_entry_end", offset = grace,
                    missing = "ineligible", when_index_source = "era_a"),
        omop_censor(eb, which = "containing_entry_end", offset = grace,
                    missing = "ineligible", when_index_source = "era_b"),
        omop_censor(eb, which = "first_start_after_entry", when_index_source = "era_a"),
        omop_censor(ea, which = "first_start_after_entry", when_index_source = "era_b")),
      same_day_priority = "event")),
  omop_output("itt_analysis", rows = "study", format = "joined", grain = "episode",
    options = list(from = list(list(output = "itt"), list(output = "baseline")),
                    how = "left", cardinality = "many_to_one")),
  omop_output("on_treatment_analysis", rows = "study", format = "joined", grain = "episode",
    options = list(from = list(list(output = "on_treatment"), list(output = "baseline")),
                    how = "left", cardinality = "many_to_one")))
publish <- c("itt_analysis", "on_treatment_analysis")
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
