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

measure <- omop_param("measure", "event")
lookback <- omop_param("lookback", "integer", default = 365L)
variables <- list(
  numeric_value = omop_variable(event = measure,
    reducer = omop_reduce("last_value", window = omop_window(-lookback, -1L),
                          missing = "keep")),
  source_records = omop_variable(event = measure,
    reducer = omop_reduce("count", window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("numeric_observation", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "numeric_observation"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
