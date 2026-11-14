events <- list()
intervals <- list()
variables <- list()
outcome <- omop_param("outcome", "event")
lookback <- omop_param("lookback", "integer", default = 365L)
populations <- list(omop_population("study",
  index = omop_index(outcome, select = "first_eligible",
                     end = omop_end("fixed", days = 0L)),
  coverage = omop_coverage(), filters = list(
    omop_observed(before = lookback),
    omop_has_not(outcome, window = omop_window(-lookback, -1L)))))
variables <- list(age = omop_age(), sex = omop_sex(),
  prior_observed_days = omop_observed_days(omop_window(-lookback, -1L)))
outputs <- list(omop_output("incident_events", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "incident_events"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
