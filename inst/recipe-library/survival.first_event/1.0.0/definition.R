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
start <- omop_param("start", "integer", default = 1L)
horizon <- omop_param("horizon", "integer", default = 365L)
prior_days <- omop_param("prior_days", "integer", default = 365L)
outputs <- list(omop_output("survival", rows = "study", format = "survival",
  grain = "episode", options = list(outcomes = list(outcome = outcome),
    tar = omop_window(start, horizon), format = "survival", event_order = "first",
    censor = list("observation_end", "death"),
    prior_outcome = list(lookback = prior_days, action = "exclude"),
    tie_policy = "priority", same_day_priority = "event")))
publish <- "survival"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
