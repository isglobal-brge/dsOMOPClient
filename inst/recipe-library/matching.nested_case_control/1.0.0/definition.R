events <- list()
intervals <- list()
variables <- list()
outcome <- omop_param("outcome", "event")
exposure <- omop_param("exposure", "event")
lookback <- omop_param("lookback", "integer", default = 365L)
m <- omop_param("controls_per_case", "integer", default = 4L)
age_width <- omop_param("age_width", "integer", default = 5L)
seed <- omop_param("seed", "integer", default = 1L)
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
