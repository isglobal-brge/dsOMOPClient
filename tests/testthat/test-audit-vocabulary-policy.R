.policy_recipe <- function() {
  expr <- list(items = list(list(concept = list(CONCEPT_ID = 201820), includeDescendants = TRUE),
    list(concept = list(CONCEPT_ID = 201821), isExcluded = TRUE)))
  r <- recipe_import_concept_sets(omop_recipe(), list(cs = expr))
  r$events <- list(e = omop_event("condition_occurrence", "cs", name = "e"))
  r$outputs <- list(o = omop_output("o", format = "long", sources = "e"))
  r
}
.policy_digest <- function(r) dsOMOPClient:::.plan_v2_digest(recipe_to_plan(r))$sha256

test_that("both imported groups retain policy in wire, save/load, code, bind and merge", {
  r <- .policy_recipe()
  plan <- recipe_to_plan(r)
  expect_identical(plan$concept_sets$cs$validity_policy, "ohdsi-import-1")
  expect_identical(plan$concept_sets$cs$exclude$validity_policy, "ohdsi-import-1")
  expect_true("vocabulary.ohdsi_import_validity" %in% dsOMOPClient:::.plan_v2_operations(plan))
  for (format in c("json", "yaml")) {
    file <- tempfile(fileext = paste0(".", format))
    recipe_save(r, file)
    expect_identical(.policy_digest(recipe_load(file)), .policy_digest(r))
    unlink(file)
  }
  expect_identical(.policy_digest(eval(parse(text = recipe_to_code(r)))), .policy_digest(r))
  parameterized <- omop_recipe(events = list(omop_event("condition_occurrence",
    omop_param("definition", type = "concept_set"), name = "e")),
    outputs = omop_output("o", format = "long", sources = "e"))
  bound <- recipe_bind(parameterized, definition = r$concept_sets$cs)
  expect_identical(recipe_to_plan(bound)$concept_sets$cs$validity_policy, "ohdsi-import-1")
  expect_identical(recipe_to_plan(bound)$concept_sets$cs$exclude$validity_policy, "ohdsi-import-1")
  merged <- recipe_merge(r, r, prefix = "copy")
  mp <- recipe_to_plan(merged)
  expect_identical(mp$concept_sets$copy_cs$validity_policy, "ohdsi-import-1")
  expect_identical(mp$concept_sets$copy_cs$exclude$validity_policy, "ohdsi-import-1")
  native <- r
  native$concept_sets$cs$validity_policy <- NULL
  native$concept_sets$cs$exclude$validity_policy <- NULL
  expect_false(identical(.policy_digest(native), .policy_digest(r)))
  expect_error(recipe_merge(r, native), "recipes disagree on concept set")
})

test_that("Circe policy survives legacy lowering and generated code", {
  expr <- list(ConceptSets = list(list(id = 0, expression = list(items = list(
    list(concept = list(CONCEPT_ID = 201820), includeDescendants = TRUE))))),
    PrimaryCriteria = list(CriteriaList = list(list(ConditionOccurrence = list(CodesetId = 0))),
      PrimaryCriteriaLimit = list(Type = "First")))
  pop <- recipe_import_circe(expr, id = "p")
  r <- omop_recipe(populations = pop, variables = omop_variable_sex(),
    outputs = omop_output("o", type = "wide", population_id = "p"))
  expect_identical(pop$index_event$validity_policy, "ohdsi-import-1")
  expect_true(dsOMOPClient:::.ohdsiHasImportedValidity(recipe_to_plan(r)))
  expect_identical(.policy_digest(eval(parse(text = recipe_to_code(r)))), .policy_digest(r))
  v1 <- recipe_to_plan(r, version = 1)
  expect_identical(v1$populations$p$index_event$concept_set$validity_policy, "ohdsi-import-1")
  expect_identical(dsOMOP:::.irPrepare(v1)$events[[1L]]$concept_set$validity_policy, "ohdsi-import-1")
  selected <- dsOMOPClient:::.recipe_select_plan(r)
  expect_identical(selected$version, 2L)
})

test_that("an incapable server refuses the distinct policy without fallback", {
  plan <- recipe_to_plan(.policy_recipe())
  ops <- dsOMOPClient:::.plan_v2_operations(plan)
  testthat::local_mocked_bindings(.v2_server_language = function(session, servers) {
    list(site = list(version = 2L, operations = setdiff(ops, "vocabulary.ohdsi_import_validity")))
  }, .package = "dsOMOPClient")
  expect_error(dsOMOPClient:::.v2_assert_supported(plan, NULL, "site"), "vocabulary.ohdsi_import_validity")
  testthat::local_mocked_bindings(.v2_server_language = function(session, servers) {
    list(site = list(version = 2L, operations = ops))
  }, .package = "dsOMOPClient")
  expect_invisible(dsOMOPClient:::.v2_assert_supported(plan, NULL, "site"))
})
