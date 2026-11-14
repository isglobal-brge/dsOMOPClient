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
horizon <- omop_param("horizon", "integer", default = 365L)
window <- omop_window(0L, horizon - 1L)
rx <- omop_event("drug_exposure", drug, end = list(fallback = "days_supply"),
                 name = "rx")
stock <- omop_era(rx, gap = 0L, persistence = 0L, min_days = 0L, by = "event",
                  end = "recorded_end", fallback = "days_supply",
                  stockpile = TRUE, name = "stock")
events <- list(rx)
intervals <- list(stock)
variables <- list(
  observed_days = omop_observed_days(window),
  recorded_covered_days = omop_variable(event = stock,
    reducer = omop_reduce("covered_days", window = window,
                          coverage = "recorded", observed_only = TRUE)),
  effective_covered_days = omop_variable(event = stock,
    reducer = omop_reduce("covered_days", window = window,
                          coverage = "effective_supply", observed_only = TRUE)),
  supply_days = omop_variable(event = rx,
    reducer = omop_reduce("supply_days", window = window, fills = "clipped")))
outputs <- list(omop_output("adherence", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "adherence"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
