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

drug <- omop_param("drug", "concept_set")
outcome <- omop_param("outcome", "event")
horizon <- omop_param("horizon", "integer", default = 365L)
age_width <- omop_param("age_width", "integer", default = 5L)
rx <- omop_event("drug_exposure", drug, name = "rx")
exposure <- omop_era(rx, gap = 0L, persistence = 0L, by = "event",
                     end = "recorded_end", fallback = "days_supply", name = "exposure")
events <- list(rx)
intervals <- list(exposure)
outputs <- list(omop_output("sccs", rows = "study", format = "person_period",
  grain = "episode", options = list(representation = "dense",
    bins = list(calendar = "year"), window = omop_window(0L, horizon),
    partition = list(boundaries = list(exposure), age_bands = age_width, calendar = "year"),
    exposures = list(drug = exposure), outcomes = list(outcome = outcome))))
publish <- "sccs"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
