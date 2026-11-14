.library_test_bindings <- function(id, defaults = FALSE) {
  values <- list(
    index = omop_event("visit_occurrence", 9201, name = "entry"),
    education = 990001L, employment = 990002L, conditions = 990004L,
    drug = 990006L, drug_a = 990006L, drug_b = 990007L,
    measure = omop_event("measurement", 990003, value = "value_as_number",
      unit = omop_unit(8840), name = "measure"),
    outcome = omop_event("condition_occurrence", 990005, name = "outcome"),
    outcome_a = omop_event("condition_occurrence", 990005, name = "cause_a"),
    outcome_b = omop_event("condition_occurrence", 990008, name = "cause_b"),
    exposure = omop_event("drug_exposure", 990006, name = "exposure"),
    source_a = omop_event("measurement", 990003, value = "value_as_number", name = "measure"),
    source_b = omop_event("condition_occurrence", 990004, name = "history"))
  params <- recipe_params(recipe_library_instantiate(id))
  result <- lapply(params, function(p) if (is.null(p$default)) values[[p$name]] else p$default)
  if (!defaults) result <- result[vapply(params, function(p) is.null(p$default), logical(1))]
  result
}
.library_test_recipe <- function(id, defaults = FALSE) {
  do.call(recipe_library_instantiate, c(list(id), .library_test_bindings(id, defaults)))
}
.library_test_dir <- function(id) testthat::test_path("fixtures", "recipe-library", id, "1.0.0", "default")
.library_test_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
.library_test_canonical <- function(x) dsOMOPClient:::.jcsCanonical(x)
.library_test_digest <- function(x) dsOMOPClient:::.plan_v2_digest(x)
