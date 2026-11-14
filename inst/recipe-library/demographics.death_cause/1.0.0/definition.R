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

horizon <- omop_param("horizon", "integer", default = 365L)
death <- omop_event("death", name = "death")
events <- list(death)
variables <- list(
  death_recorded = omop_variable(event = death,
    reducer = omop_reduce("binary", window = omop_window(0L, horizon))),
  death_days = omop_variable(event = death,
    reducer = omop_reduce("first_date", window = omop_window(0L, horizon))),
  cause_concept_id = omop_variable(event = death,
    reducer = omop_reduce("first_value", value = "concept_id", window = omop_window(0L, horizon),
                          missing = "keep")))
outputs <- list(omop_output("death_cause", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "death_cause"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
