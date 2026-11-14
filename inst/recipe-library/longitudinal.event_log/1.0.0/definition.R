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

source_a <- omop_param("source_a", "event")
source_b <- omop_param("source_b", "event")
outputs <- list(omop_output("event_log", rows = "study", format = "long",
  grain = "episode", sources = list(source_a, source_b),
  options = list(event_log = TRUE, unit = "episode_event", date_handling = "relative")))
publish <- "event_log"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
