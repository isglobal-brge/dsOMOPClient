# Literal control transcribed from the endorsed section-4 expansion.
# Synthetic binding constants are study fixtures, not catalogue defaults.
events <- list()
intervals <- list()
variables <- list()
drug <- 990006L
washout <- 365L
select <- "first_eligible"
by <- "ingredient"
# A-23 literal lowering: do not call the helper being checked.
rx <- omop_event("drug_exposure", drug)
era <- omop_era(rx, gap = 30L, persistence = 0L, min_days = 0L,
  by = by, unmapped = "own_concept", end = "recorded_end",
  fallback = "days_supply", stockpile = FALSE)
filters <- if (washout == 0L) list() else list(
  omop_observed(before = washout),
  omop_has_not(rx, window = omop_window(-washout, -1L)))
nu <- omop_population("new_users", label = "New users",
  index = omop_index(era, select = select, end = omop_end("interval_end")),
  filters = filters)
populations <- list(nu)
variables <- list(age = omop_age(), sex = omop_sex(),
                   index_source = omop_population_column("index_source"))
outputs <- list(omop_output("new_users", rows = "new_users", format = "wide",
  grain = "episode", columns = names(variables),
  options = list(date_handling = "relative")))
publish <- "new_users"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
