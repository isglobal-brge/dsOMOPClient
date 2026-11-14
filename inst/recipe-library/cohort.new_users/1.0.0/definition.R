events <- list()
intervals <- list()
variables <- list()
drug <- omop_param("drug", "concept_set")
washout <- omop_param("washout_days", "integer", default = 365L)
select <- omop_param("select", "string", default = "first_eligible",
                     description = "Selection among index candidates")
by <- omop_param("by", "string", default = "ingredient")
nu <- omop_new_users(drug, washout_days = washout, select = select, by = by)
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
