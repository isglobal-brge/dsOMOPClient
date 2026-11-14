# Literal control transcribed from the endorsed section-4 expansion.
# Synthetic binding constants are study fixtures, not catalogue defaults.
events <- list()
intervals <- list()
variables <- list()
outcome <- omop_event("condition_occurrence", 990005, name = "outcome")
exposure <- omop_event("drug_exposure", 990006, name = "exposure")
lookback <- 365L
m <- 4L
age_width <- 5L
seed <- 1L
populations <- list(
  omop_population("cases",
    index = omop_index(outcome, select = "first_eligible",
                       end = omop_end("fixed", days = 0L)),
    coverage = omop_coverage(), filters = list(
      omop_observed(before = lookback),
      omop_has_not(outcome, window = omop_window(NULL, -1L)))),
  omop_population("controls", coverage = omop_coverage(),
    filters = list(omop_observed(before = lookback))),
  omop_population("matched", match = omop_match(cases = "cases", controls = "controls",
    outcome = outcome, at = "index", match = list("sex", omop_age_band(age_width)),
    controls_per_case = m, prior_outcome = "exclude", future_cases = "eligible",
    replacement = "between_sets", seed = seed, insufficient = "keep_available",
    missing_strata = "ineligible", max_candidates = 1000000L)))
variables <- list(set_id = omop_population_column("set_id"),
  is_case = omop_population_column("is_case"),
  sampling_weight = omop_population_column("sampling_weight"),
  age = omop_age(), sex = omop_sex(),
  exposure_history = omop_variable(event = exposure,
    reducer = omop_reduce("binary", window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("matched", rows = "matched", format = "wide",
  grain = "episode", columns = names(variables),
  options = list(date_handling = "relative")))
publish <- "matched"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
