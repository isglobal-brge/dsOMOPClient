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
horizon <- omop_param("horizon", "integer", default = 365L)
bin_days <- omop_param("bin_days", "integer", default = 30L)
lookback <- omop_param("lookback", "integer", default = 90L)
expire <- omop_param("expire", "integer", default = 30L)
outputs <- list(omop_output("measurements", rows = "study", format = "person_period",
  grain = "episode", options = list(representation = "dense", sources = list(measure),
    bins = list(width = bin_days), window = omop_window(0L, horizon),
    analyses = c("count", "mean", "last"),
    carry_forward = list(measure = list(lookback = lookback, expire = expire)))))
publish <- "measurements"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
