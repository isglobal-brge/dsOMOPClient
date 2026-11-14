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
washout <- omop_param("washout_days", "integer", default = 0L)
outputs <- list(omop_output("recurrent", rows = "study", format = "survival",
  grain = "episode", options = list(outcomes = list(outcome = outcome),
    tar = omop_window(1L, horizon), format = "recurrent_events", event_order = "all",
    washout_days = washout, censor = list("observation_end", "death"),
    tie_policy = "priority", same_day_priority = "event", complete = TRUE)))
publish <- "recurrent"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
