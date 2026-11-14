# Reproduce two released-2.10.4 edge cases encountered while writing Book 2.
# These probes are evidence for a future runtime job, not analyst lessons.
library(dsOMOPClient)
source("tools/book/fixture.R")
f <- book_fixture(); conns <- f$conns
tryCatch({
  message("CLIENT ", as.character(packageVersion("dsOMOPClient")),
          " SERVER ", as.character(packageVersion("dsOMOP")))
  message("PROBE: explicit singleton variable named date")
  date_result <- tryCatch({
    r <- omop_recipe(variables=list(date=omop_date("first_visit")),
      outputs=omop_output("people",format="wide",grain="person"))
    stopifnot(recipe_explain(r)$selected=="v2")
    recipe_execute(r,out=c(people="D_probe_date_name"),conns=conns)
    "Unexpected success"
  }, error=conditionMessage)
  message(date_result)
  stopifnot(grepl("date_source_invalid",date_result))
  good <- omop_recipe(variables=list(visit_date=omop_date("first_visit")),
    outputs=omop_output("people",format="wide",grain="person"))
  stopifnot(recipe_explain(good)$selected=="v2")
  print(recipe_execute(good,out=c(people="D_probe_date"),conns=conns))
  message("PROBE: exact-age variable carried into temporal covariates")
  visits <- omop_event("visit_occurrence",c(9201L,9202L),name="visits")
  temporal <- omop_recipe(events=list(visits),
    populations=list(omop_population("visitors",index=omop_index(visits))),
    variables=list(age=omop_age_at("index")),
    outputs=omop_output("temporal",rows="visitors",format="temporal_covariates",
      options=list(sources=list(visits),bin_width=30L,
        window=omop_window(0,89),analyses="count")))
  stopifnot(recipe_explain(temporal)$selected=="v2")
  temporal_result <- tryCatch({
    recipe_execute(temporal,out=c(temporal="D_probe_temporal"),conns=conns)
    "Unexpected success"
  },error=conditionMessage)
  message(temporal_result)
  stopifnot(grepl("date_spec_invalid",temporal_result))
  # A separate wide output carries completed age. Sparse event/time coordinates
  # need no such variable: remove it for that standalone output.
  temporal$variables <- NULL
  print(recipe_execute(temporal,out=c(temporal="D_probe_coordinates"),conns=conns))
}, finally={
  ds.omop.disconnect(conns=conns); DSI::datashield.logout(conns); unlink(f$path)
})
