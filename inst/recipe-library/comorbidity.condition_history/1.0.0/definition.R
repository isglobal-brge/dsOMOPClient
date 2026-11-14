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

conditions <- omop_param("conditions", "concept_set")
lookback <- omop_param("lookback", "integer", default = 365L)
dx <- omop_event("condition_occurrence", conditions, name = "history")
events <- list(dx)
variables <- list(
  history_present = omop_variable(event = dx,
    reducer = omop_reduce("binary", window = omop_window(-lookback, -1L))),
  history_days = omop_variable(event = dx,
    reducer = omop_reduce("count", distinct = "day",
                          window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("history", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "history"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
