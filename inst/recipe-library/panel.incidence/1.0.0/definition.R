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

outcome <- omop_param("outcome", "event")
horizon <- omop_param("horizon", "integer", default = 365L)
outputs <- list(omop_output("incidence", rows = "study", format = "person_period",
  grain = "episode", options = list(representation = "dense",
    bins = list(calendar = "year"), window = omop_window(0L, horizon),
    outcomes = list(outcome = outcome))))
publish <- "incidence"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
