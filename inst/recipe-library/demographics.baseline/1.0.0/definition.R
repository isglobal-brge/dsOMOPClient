events <- list()
intervals <- list()
variables <- list()
index <- omop_param("index", "event", description = "Event or era defining entry")
select <- omop_param("select", "string", default = "first_eligible",
                     description = "Selection among index candidates")
populations <- list(omop_population("study",
  index = omop_index(index, select = select,
                     end = omop_end("observation_end")),
  coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                           on_missing = "drop", gap = 0L)))

lookback <- omop_param("lookback", "integer", default = 365L)
variables <- list(
  age = omop_age(reference = "index", bands = FALSE),
  sex = omop_sex(), race = omop_race(), ethnicity = omop_ethnicity(),
  prior_observed_days = omop_observed_days(omop_window(-lookback, -1L)))
outputs <- list(omop_output("baseline", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "baseline"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
