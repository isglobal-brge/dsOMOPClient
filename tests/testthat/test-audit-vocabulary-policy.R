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

test_that("all legal registry names require imported-policy capability", {
  for (name in c("ordinary", "meta", "metadata", "circe_provenance")) {
    for (registry in c(FALSE, TRUE)) {
      cs <- omop_concept_set(2147483647L, validity_policy = "ohdsi-import-1",
        name = if (registry) name else NULL)
      recipe <- omop_recipe(events = list(omop_event("condition_occurrence", cs, name = name)),
        outputs = omop_output("o", format = "long", sources = name))
      plan <- recipe_to_plan(recipe, version = 2)
      if (registry) expect_true(name %in% names(plan$concept_sets))
      expect_true(dsOMOPClient:::.ohdsiHasImportedValidity(recipe))
      ops <- dsOMOPClient:::.plan_v2_operations(plan)
      expect_true("vocabulary.ohdsi_import_validity" %in% ops)
      local({
        testthat::local_mocked_bindings(.v2_server_language = function(session, servers) {
          list(site = list(version = 2L, operations = setdiff(ops, "vocabulary.ohdsi_import_validity")))
        }, .package = "dsOMOPClient")
        expect_error(dsOMOPClient:::.v2_assert_supported(plan, NULL, "site"),
          "vocabulary.ohdsi_import_validity")
      })
    }
  }
})

test_that("Circe origin and wire meta cannot introduce executable policy requirements", {
  decoy <- list(concepts = list(2147483647), validity_policy = "ohdsi-import-1")
  native <- omop_recipe(events = list(omop_event("condition_occurrence", 201820L, name = "meta")),
    outputs = omop_output("o", format = "long", sources = "meta"))
  plan <- recipe_to_plan(native)
  plan$meta$example <- decoy
  expect_false(dsOMOPClient:::.ohdsiHasImportedValidity(plan))
  pop <- omop_population(id = "circe_provenance", index_event = omop_index_event(201820L, table = "condition_occurrence"))
  pop$circe_provenance <- decoy
  native <- omop_recipe(populations = pop)
  expect_false(dsOMOPClient:::.ohdsiHasImportedValidity(native))
  native$populations$circe_provenance$index_event$validity_policy <- "ohdsi-import-1"
  expect_true(dsOMOPClient:::.ohdsiHasImportedValidity(native))
})

test_that("imported validity survives category-domain composition after prerequisite integration", {
  domain <- omop_concept_set(201820L, descendants = TRUE,
    exclude = omop_concept_set(201821L, validity_policy = "ohdsi-import-1"),
    validity_policy = "ohdsi-import-1")
  category <- omop_category_spec(rollup = omop_category_rollup("ancestors",
    ancestors = "concept:201820", vocabulary = "SNOMED", domain = domain))
  r <- omop_recipe(variables = list(x = omop_category_rows(
    omop_event("condition_occurrence", 201820L), category_spec = category)),
    outputs = omop_output("o", format = "long", sources = omop_event("condition_occurrence", 201820L)))
  plan <- recipe_to_plan(r)
  declared <- plan$variables$x$category_spec$rollup$domain
  expect_identical(declared$validity_policy, "ohdsi-import-1")
  expect_identical(declared$exclude$validity_policy, "ohdsi-import-1")
  expect_true("vocabulary.ohdsi_import_validity" %in% .plan_v2_operations(plan))
  expect_true("vocabulary.ohdsi_import_validity" %in% dsOMOP:::.irOperations(dsOMOP:::.irPrepare(plan)))
  expect_identical(.policy_digest(recipe_import_json(recipe_export_json(r))), .policy_digest(r))
  expect_identical(.policy_digest(eval(parse(text = recipe_to_code(r)))), .policy_digest(r))
  bad <- declared; bad$validity_policy <- "PRIVATE_SENTINEL"
  expect_error(.categoryDomainCheck(bad), "^invalid_vocabulary_policy:")
})
