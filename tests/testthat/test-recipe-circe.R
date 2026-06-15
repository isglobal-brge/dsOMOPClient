# ==============================================================================
# Tests for the recipe <-> OHDSI Circe (ATLAS) cohort-expression interop and the
# recipe schema-version reset / tolerant reader (Phase 7c).
# ==============================================================================

# --- helpers ------------------------------------------------------------------

# Find the first filter of a given type in a (flat) population filter list.
.find_filter <- function(filters, type) {
  hit <- Filter(function(f) identical(f$type, type), filters)
  if (length(hit) == 0) NULL else hit[[1]]
}
.filter_types <- function(filters) {
  vapply(filters, function(f) {
    if (inherits(f, "omop_filter_group")) paste0("group:", f$operator)
    else f$type %||% "?"
  }, character(1))
}

# A rich population exercising every SUPPORTED Circe construct at once.
.supported_recipe <- function() {
  omop_recipe(
    populations = omop_population(
      id = "t2d", label = "Type 2 diabetes, female, 18-65",
      filters = list(
        omop_filter_has_concept(201820, "condition_occurrence"),       # anchor
        omop_filter_sex("F"),                                          # demographic
        omop_filter_age(18, 65),                                       # demographic
        omop_filter_has_concept(1503297, "drug_exposure",
                                min_count = 2L),                       # -> concept_count
        omop_filter_not_has_concept(443238, "condition_occurrence"),    # exclusion
        omop_filter_has_measurement(3004410, min_value = 6.5,
                                    max_value = 10),                   # value range
        omop_filter_prior_observation(365),                           # obs window prior
        omop_filter_followup(30),                                     # obs window post
        omop_filter_group(                                            # OR group
          omop_filter_has_concept(316866, "condition_occurrence"),
          omop_filter_has_concept(4329847, "condition_occurrence"),
          operator = "OR")
      )),
    outputs = omop_output(type = "wide", population_id = "t2d"))
}

# ==============================================================================
# (VERSION) schema-version reset + tolerant reader
# ==============================================================================

test_that("a newly saved recipe stamps schema version '1'", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, name = "yob", table = "person",
                                          column = "year_of_birth",
                                          type = "numeric")
  json <- recipe_export_json(r)
  expect_equal(jsonlite::fromJSON(json, simplifyVector = FALSE)$version, "1")

  yaml <- recipe_export_yaml(r)
  expect_equal(yaml::yaml.load(yaml)$version, "1")
})

test_that("an unknown (newer) schema version warns but still loads", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, name = "yob", table = "person",
                                          column = "year_of_birth",
                                          type = "numeric")
  future_json <- sub("\"version\": ?\"1\"", "\"version\": \"99.0\"",
                     recipe_export_json(r))

  expect_warning(loaded <- recipe_import_json(future_json),
                 "not recognized")
  expect_s3_class(loaded, "omop_recipe")
  expect_true("yob" %in% names(loaded$variables))
})

test_that("recipe_save -> recipe_load round-trips unaffected by the version reset", {
  r <- omop_recipe(
    populations = omop_population(id = "adults", label = "Adults",
                                 parent_id = "base",
                                 filters = list(omop_filter_age(18, 80))),
    outputs = omop_output(name = "o", type = "wide", population_id = "adults"))

  for (ext in c(".json", ".yml")) {
    path <- tempfile(fileext = ext)
    on.exit(unlink(path), add = TRUE)
    recipe_save(r, path)
    loaded <- recipe_load(path)
    expect_s3_class(loaded, "omop_recipe")
    expect_true("adults" %in% names(loaded$populations))
    expect_equal(length(loaded$populations$adults$filters), 1)
  }
})

# ==============================================================================
# (CIRCE export) well-formed cohort-expression JSON
# ==============================================================================

test_that("recipe_export_circe emits well-formed Circe JSON for supported constructs", {
  json <- recipe_export_circe(.supported_recipe(), population_id = "t2d")
  expect_type(json, "character")

  expr <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  # The shape ATLAS expects.
  expect_true(all(c("ConceptSets", "PrimaryCriteria", "InclusionRules") %in%
                    names(expr)))

  # ConceptSets are well-formed (id + items carrying CONCEPT_IDs).
  expect_gt(length(expr$ConceptSets), 0)
  for (s in expr$ConceptSets) {
    expect_true(!is.null(s$id))
    expect_gt(length(s$expression$items), 0)
    expect_true(!is.null(s$expression$items[[1]]$concept$CONCEPT_ID))
  }

  # PrimaryCriteria entry event = the first positive has_concept (condition).
  pc <- expr$PrimaryCriteria$CriteriaList
  expect_equal(length(pc), 1)
  expect_equal(names(pc[[1]]), "ConditionOccurrence")

  # Observation window came from prior_observation / followup.
  expect_equal(as.integer(expr$PrimaryCriteria$ObservationWindow$PriorDays), 365L)
  expect_equal(as.integer(expr$PrimaryCriteria$ObservationWindow$PostDays), 30L)

  # InclusionRules carry the non-anchor criteria; each is a well-formed rule.
  expect_gt(length(expr$InclusionRules), 0)
  for (rule in expr$InclusionRules) {
    expect_true(!is.null(rule$name))
    expect_true(!is.null(rule$expression$Type))
  }
})

test_that("recipe_export_circe maps a single-positive demographics cohort to an 'any visit' entry", {
  r <- omop_recipe(
    populations = omop_population(id = "f", label = "Females 40+",
      filters = list(omop_filter_sex("F"), omop_filter_age(40, 150))),
    outputs = omop_output(type = "wide", population_id = "f"))
  expr <- jsonlite::fromJSON(recipe_export_circe(r, "f"), simplifyVector = FALSE)
  # No positive event -> permissive VisitOccurrence entry placeholder.
  expect_equal(names(expr$PrimaryCriteria$CriteriaList[[1]]), "VisitOccurrence")
  # Demographics surface as a DemographicCriteriaList inclusion rule.
  demo_rule <- Filter(function(rl) length(rl$expression$DemographicCriteriaList) > 0,
                      expr$InclusionRules)
  expect_equal(length(demo_rule), 1)
})

# ==============================================================================
# (CIRCE round-trip) supported subset is lossless
# ==============================================================================

test_that("the supported Circe subset round-trips to an equivalent recipe population", {
  pop <- recipe_import_circe(recipe_export_circe(.supported_recipe(), "t2d"))
  expect_s3_class(pop, "omop_population")

  # Round-trip hint preserves identity.
  expect_equal(pop$id, "t2d")
  expect_equal(pop$label, "Type 2 diabetes, female, 18-65")

  types <- .filter_types(pop$filters)
  # All nine supported constructs are reconstructed (a non-anchor has_concept
  # with min_count > 1 is recovered as the equivalent concept_count).
  expect_true("has_concept" %in% types)        # anchor
  expect_true("concept_count" %in% types)       # drug min_count = 2
  expect_true("not_has_concept" %in% types)
  expect_true("has_measurement" %in% types)
  expect_true("sex" %in% types)
  expect_true("age_range" %in% types)
  expect_true("prior_observation" %in% types)
  expect_true("followup" %in% types)
  expect_true("group:OR" %in% types)

  # Anchor: concept + table recovered.
  anchor <- .find_filter(pop$filters, "has_concept")
  expect_equal(anchor$params$concept_id, 201820L)
  expect_equal(anchor$params$table, "condition_occurrence")

  # Counts / exclusion recovered with the right semantics.
  expect_equal(.find_filter(pop$filters, "concept_count")$params$min_count, 2L)
  expect_equal(.find_filter(pop$filters, "not_has_concept")$params$concept_id,
               443238L)

  # Measurement value range recovered.
  meas <- .find_filter(pop$filters, "has_measurement")
  expect_equal(meas$params$min_value, 6.5)
  expect_equal(meas$params$max_value, 10)

  # Demographics recovered.
  expect_equal(.find_filter(pop$filters, "sex")$params$value, "F")
  age <- .find_filter(pop$filters, "age_range")
  expect_equal(age$params$min, 18L)
  expect_equal(age$params$max, 65L)

  # Observation windows recovered.
  expect_equal(.find_filter(pop$filters, "prior_observation")$params$min_days,
               365L)
  expect_equal(.find_filter(pop$filters, "followup")$params$min_days, 30L)

  # OR group recovered with two has_concept members.
  grp <- Filter(function(f) inherits(f, "omop_filter_group"), pop$filters)[[1]]
  expect_equal(grp$operator, "OR")
  expect_equal(length(grp$children), 2)
  expect_equal(vapply(grp$children, function(ch) ch$type, character(1)),
               c("has_concept", "has_concept"))

  # The imported population is RUNNABLE: it compiles into a recipe plan.
  r2 <- omop_recipe(populations = pop,
                    outputs = omop_output(type = "wide", population_id = "t2d"))
  expect_silent(plan <- dsOMOPClient:::recipe_to_plan(r2))
})

test_that("multi-concept codesets round-trip and dedup shared concept sets", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      filters = list(
        omop_filter_has_concept(c(201820, 201826), "condition_occurrence"),
        # Same id vector reused -> should share one concept set on export.
        omop_filter_not_has_concept(c(201820, 201826), "condition_occurrence"))),
    outputs = omop_output(type = "wide", population_id = "p"))
  json <- recipe_export_circe(r, "p")
  expr <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(length(expr$ConceptSets), 1)  # deduplicated

  pop <- recipe_import_circe(json)
  expect_equal(.find_filter(pop$filters, "has_concept")$params$concept_id,
               c(201820L, 201826L))
})

test_that("measurement one-sided value bounds (gte / lte) round-trip losslessly", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      filters = list(
        omop_filter_has_concept(201820, "condition_occurrence"),
        omop_filter_has_measurement(3004410, min_value = 6.5),   # gte
        omop_filter_has_measurement(3013682, max_value = 140))), # lte
    outputs = omop_output(type = "wide", population_id = "p"))
  pop <- recipe_import_circe(recipe_export_circe(r, "p"))
  ms <- Filter(function(f) identical(f$type, "has_measurement"), pop$filters)
  gte <- Filter(function(m) identical(as.integer(m$params$concept_id), 3004410L),
                ms)[[1]]
  lte <- Filter(function(m) identical(as.integer(m$params$concept_id), 3013682L),
                ms)[[1]]
  expect_equal(gte$params$min_value, 6.5)
  expect_null(gte$params$max_value)
  expect_null(lte$params$min_value)
  expect_equal(lte$params$max_value, 140)   # regression: lte bound must survive
})

test_that("non-anchor not_has_concept windows round-trip", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      filters = list(
        omop_filter_has_concept(201820, "condition_occurrence"),
        omop_filter_not_has_concept(443238, "condition_occurrence",
                                    window = list(start = -30, end = 0)))),
    outputs = omop_output(type = "wide", population_id = "p"))
  pop <- recipe_import_circe(recipe_export_circe(r, "p"))
  nh <- .find_filter(pop$filters, "not_has_concept")
  expect_equal(nh$params$window$start, -30L)
  expect_equal(nh$params$window$end, 0L)
})

# ==============================================================================
# (CIRCE set-ops) union -> ANY, intersect -> ALL
# ==============================================================================

test_that("set-op populations export to a top-level CriteriaGroup (union ANY / intersect ALL)", {
  ru <- omop_recipe(populations = list(
    omop_population(id = "a", label = "a",
      filters = list(omop_filter_has_concept(201820, "condition_occurrence"))),
    omop_population(id = "b", label = "b",
      filters = list(omop_filter_has_concept(316866, "condition_occurrence"))),
    omop_population(id = "u", label = "u", union = c("a", "b"))),
    outputs = omop_output(type = "wide", population_id = "u"))
  eu <- jsonlite::fromJSON(recipe_export_circe(ru, "u"), simplifyVector = FALSE)
  ug <- Filter(function(x) grepl("Set-op", x$name), eu$InclusionRules)[[1]]
  expect_equal(ug$expression$Type, "ANY")
  expect_equal(length(ug$expression$CriteriaList), 2)

  ri <- omop_recipe(populations = list(
    omop_population(id = "a", label = "a",
      filters = list(omop_filter_has_concept(201820, "condition_occurrence"))),
    omop_population(id = "b", label = "b",
      filters = list(omop_filter_has_concept(316866, "condition_occurrence"))),
    omop_population(id = "i", label = "i", intersect = c("a", "b"))),
    outputs = omop_output(type = "wide", population_id = "i"))
  ei <- jsonlite::fromJSON(recipe_export_circe(ri, "i"), simplifyVector = FALSE)
  ig <- Filter(function(x) grepl("Set-op", x$name), ei$InclusionRules)[[1]]
  expect_equal(ig$expression$Type, "ALL")
})

# ==============================================================================
# (CIRCE unsupported) warned, never silently dropped
# ==============================================================================

test_that("an unsupported filter type warns on Circe export (not silently dropped)", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      filters = list(
        omop_filter_has_concept(201820, "condition_occurrence"),
        omop_filter_visit_count(2))),
    outputs = omop_output(type = "wide", population_id = "p"))
  expect_warning(recipe_export_circe(r, "p"),
                 "visit_count.*no Circe analog")
})

test_that("an unsupported recipe table warns on Circe export", {
  # death has no Circe domain analog.
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      filters = list(omop_filter_has_concept(4306655, "death"))),
    outputs = omop_output(type = "wide", population_id = "p"))
  expect_warning(recipe_export_circe(r, "p"), "no Circe domain analog")
})

test_that("setdiff populations warn and export the first member only", {
  r <- omop_recipe(populations = list(
    omop_population(id = "a", label = "a",
      filters = list(omop_filter_has_concept(201820, "condition_occurrence"))),
    omop_population(id = "b", label = "b",
      filters = list(omop_filter_has_concept(316866, "condition_occurrence"))),
    omop_population(id = "d", label = "d", setdiff = c("a", "b"))),
    outputs = omop_output(type = "wide", population_id = "d"))
  expect_warning(json <- recipe_export_circe(r, "d"), "setdiff")
  # First member's criteria are present (not silently lost).
  expr <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_gt(length(expr$ConceptSets), 0)
})

test_that("a cohort_definition_id reference warns on Circe export", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      cohort_definition_id = 42L),
    outputs = omop_output(type = "wide", population_id = "p"))
  expect_warning(recipe_export_circe(r, "p"), "cohort_definition_id")
})

test_that("Circe-only EndStrategy / CensoringCriteria warn on import (dropped)", {
  circe <- jsonlite::toJSON(list(
    ConceptSets = list(list(id = 0L, name = "DM",
      expression = list(items = list(list(concept = list(CONCEPT_ID = 201820L)))))),
    PrimaryCriteria = list(
      CriteriaList = list(list(ConditionOccurrence = list(CodesetId = 0L))),
      ObservationWindow = list(PriorDays = 0L, PostDays = 0L),
      PrimaryCriteriaLimit = list(Type = "First")),
    InclusionRules = list(),
    EndStrategy = list(DateOffset = list(Offset = 30L)),
    CensoringCriteria = list()), auto_unbox = TRUE)
  expect_warning(recipe_import_circe(circe), "EndStrategy")
})

test_that("nested CriteriaGroups beyond one level warn on import (dropped)", {
  circe <- jsonlite::toJSON(list(
    ConceptSets = list(list(id = 0L, name = "DM",
      expression = list(items = list(list(concept = list(CONCEPT_ID = 201820L)))))),
    PrimaryCriteria = list(
      CriteriaList = list(list(ConditionOccurrence = list(CodesetId = 0L))),
      ObservationWindow = list(PriorDays = 0L, PostDays = 0L),
      PrimaryCriteriaLimit = list(Type = "First")),
    InclusionRules = list(list(name = "nested",
      expression = list(Type = "ALL", CriteriaList = list(),
        DemographicCriteriaList = list(),
        Groups = list(list(Type = "ANY", CriteriaList = list(),
                           DemographicCriteriaList = list(),
                           Groups = list())))))), auto_unbox = TRUE)
  expect_warning(recipe_import_circe(circe), "nested CriteriaGroups")
})

# ==============================================================================
# (CIRCE external) an externally-authored minimal Circe imports to a runnable pop
# ==============================================================================

test_that("an externally-authored minimal Circe JSON imports to a runnable recipe population", {
  # Hand-written (no .dsomop round-trip hint), as ATLAS would emit: a diabetes
  # entry event + an age 18-65 demographic inclusion rule + a prior-obs window.
  external <- paste0(
    '{"ConceptSets":[{"id":0,"name":"Diabetes",',
    '"expression":{"items":[{"concept":{"CONCEPT_ID":201820}}]}}],',
    '"PrimaryCriteria":{"CriteriaList":[{"ConditionOccurrence":{"CodesetId":0}}],',
    '"ObservationWindow":{"PriorDays":365,"PostDays":0},',
    '"PrimaryCriteriaLimit":{"Type":"First"}},',
    '"InclusionRules":[{"name":"adult","expression":{"Type":"ALL",',
    '"CriteriaList":[],"DemographicCriteriaList":[{"Age":{"Value":18,',
    '"Extent":65,"Op":"bt"}}],"Groups":[]}}]}')

  pop <- recipe_import_circe(external, id = "ext", label = "External cohort")
  expect_s3_class(pop, "omop_population")
  expect_equal(pop$id, "ext")
  expect_equal(pop$label, "External cohort")

  types <- .filter_types(pop$filters)
  expect_true("has_concept" %in% types)
  expect_true("prior_observation" %in% types)
  expect_true("age_range" %in% types)

  anchor <- .find_filter(pop$filters, "has_concept")
  expect_equal(anchor$params$concept_id, 201820L)
  expect_equal(anchor$params$table, "condition_occurrence")
  expect_equal(.find_filter(pop$filters, "prior_observation")$params$min_days,
               365L)
  age <- .find_filter(pop$filters, "age_range")
  expect_equal(age$params$min, 18L)
  expect_equal(age$params$max, 65L)

  # Runnable: drop it into a recipe and compile a plan.
  r <- omop_recipe(populations = pop,
                   outputs = omop_output(type = "wide", population_id = "ext"))
  expect_silent(dsOMOPClient:::recipe_to_plan(r))
})

test_that("recipe_export_circe errors for an unknown population_id", {
  expect_error(recipe_export_circe(omop_recipe(), population_id = "nope"),
               "not found")
})
