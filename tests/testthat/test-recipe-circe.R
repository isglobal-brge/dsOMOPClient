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
      index_event = omop_index_event(201820, "condition_occurrence"),
      filters = list(
        omop_filter_sex("F"),                                          # demographic
        omop_filter_age(18, 65),                                       # demographic
        omop_filter_has_concept(1503297, "drug_exposure",
                                min_count = 2L),                       # -> concept_count
        omop_filter_not_has_concept(443238, "condition_occurrence"),    # exclusion
        omop_filter_has_measurement(3004410),                         # presence
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

  # PrimaryCriteria entry event = the explicit index event (condition).
  pc <- expr$PrimaryCriteria$CriteriaList
  expect_equal(length(pc), 1)
  expect_equal(names(pc[[1]]), "ConditionOccurrence")
  # Empty means Circe's default exit: the covering observation-period end.
  expect_length(expr$EndStrategy, 0L)

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

test_that("recipe_export_circe never invents an implicit entry event", {
  r <- omop_recipe(
    populations = omop_population(id = "f", label = "Females 40+",
      filters = list(omop_filter_sex("F"), omop_filter_age(40, 150))),
    outputs = omop_output(type = "wide", population_id = "f"))
  expect_error(recipe_export_circe(r, "f"), "explicit omop_index_event")
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
  expect_s3_class(pop$index_event, "omop_index_event")
  expect_equal(pop$index_event$concept_id, 201820L)
  expect_equal(pop$index_event$table, "condition_occurrence")
  # Inclusion filters are reconstructed without duplicating the primary event.
  expect_false("has_concept" %in% types)
  expect_true("concept_count" %in% types)       # drug min_count = 2
  expect_true("not_has_concept" %in% types)
  expect_true("has_measurement" %in% types)
  expect_true("sex" %in% types)
  expect_true("age_range" %in% types)
  expect_true("prior_observation" %in% types)
  expect_true("followup" %in% types)
  expect_true("group:OR" %in% types)

  # Counts / exclusion recovered with the right semantics.
  expect_equal(.find_filter(pop$filters, "concept_count")$params$min_count, 2L)
  expect_equal(.find_filter(pop$filters, "not_has_concept")$params$concept_id,
               443238L)

  # Presence-only measurement recovered.
  meas <- .find_filter(pop$filters, "has_measurement")
  expect_null(meas$params$min_value)
  expect_null(meas$params$max_value)

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
                    variables = omop_variable_age(),
                    outputs = omop_output(type = "wide", population_id = "t2d"))
  expect_silent(plan <- dsOMOPClient:::recipe_to_plan(r2))
})

test_that("multi-concept codesets round-trip and dedup shared concept sets", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      index_event = omop_index_event(c(201820, 201826),
                                     "condition_occurrence"),
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

test_that("numeric measurement bounds fail closed on Circe export", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      index_event = omop_index_event(201820, "condition_occurrence"),
      filters = list(
        omop_filter(type = "has_measurement", level = "population",
          params = list(concept_id = 3004410L, min_value = 6.5,
                        max_value = NULL, safe_scope = NULL)),   # gte
        omop_filter(type = "has_measurement", level = "population",
          params = list(concept_id = 3013682L, min_value = NULL,
                        max_value = 140, safe_scope = NULL)))),  # lte
    outputs = omop_output(type = "wide", population_id = "p"))
  expect_true("unbound_numeric_filter" %in% recipe_lint(r)$code)
  expect_error(recipe_export_circe(r, "p"), "safe-bin contract")
})

test_that("non-anchor not_has_concept windows round-trip", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      index_event = omop_index_event(201820, "condition_occurrence"),
      filters = list(
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

test_that("set-op populations fail closed on Circe export", {
  ru <- omop_recipe(populations = list(
    omop_population(id = "a", label = "a",
      filters = list(omop_filter_has_concept(201820, "condition_occurrence"))),
    omop_population(id = "b", label = "b",
      filters = list(omop_filter_has_concept(316866, "condition_occurrence"))),
    omop_population(id = "u", label = "u", union = c("a", "b"))),
    outputs = omop_output(type = "wide", population_id = "u"))
  expect_error(recipe_export_circe(ru, "u"), "set-operation populations")

  ri <- omop_recipe(populations = list(
    omop_population(id = "a", label = "a",
      filters = list(omop_filter_has_concept(201820, "condition_occurrence"))),
    omop_population(id = "b", label = "b",
      filters = list(omop_filter_has_concept(316866, "condition_occurrence"))),
    omop_population(id = "i", label = "i", intersect = c("a", "b"))),
    outputs = omop_output(type = "wide", population_id = "i"))
  expect_error(recipe_export_circe(ri, "i"), "set-operation populations")
})

# ==============================================================================
# (CIRCE unsupported) warned, never silently dropped
# ==============================================================================

test_that("an unsupported filter type errors on Circe export", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      index_event = omop_index_event(201820, "condition_occurrence"),
      filters = list(
        omop_filter_visit_count(2))),
    outputs = omop_output(type = "wide", population_id = "p"))
  expect_error(recipe_export_circe(r, "p"), "visit_count.*outside")
})

test_that("an unsupported recipe table errors on Circe export", {
  # death has no Circe domain analog.
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      index_event = omop_index_event(201820, "condition_occurrence"),
      filters = list(omop_filter_has_concept(4306655, "death"))),
    outputs = omop_output(type = "wide", population_id = "p"))
  expect_error(recipe_export_circe(r, "p"), "outside the executable")
})

test_that("setdiff populations fail closed", {
  r <- omop_recipe(populations = list(
    omop_population(id = "a", label = "a",
      filters = list(omop_filter_has_concept(201820, "condition_occurrence"))),
    omop_population(id = "b", label = "b",
      filters = list(omop_filter_has_concept(316866, "condition_occurrence"))),
    omop_population(id = "d", label = "d", setdiff = c("a", "b"))),
    outputs = omop_output(type = "wide", population_id = "d"))
  expect_error(recipe_export_circe(r, "d"), "set-operation populations")
})

test_that("a cohort_definition_id reference fails closed on Circe export", {
  r <- omop_recipe(
    populations = omop_population(id = "p", label = "p",
      cohort_definition_id = 42L),
    outputs = omop_output(type = "wide", population_id = "p"))
  expect_error(recipe_export_circe(r, "p"), "cohort_definition_id")
})

test_that("supported Circe DateOffset round-trips without changing shape", {
  strategies <- list(
    list(DateOffset = list(DateField = "StartDate", Offset = 30L)),
    list(DateOffset = list(DateField = "EndDate", Offset = 0L)),
    list(DateOffset = list(DateField = "EndDate", Offset = -7L))
  )
  for (strategy in strategies) {
    r <- omop_recipe(
      populations = omop_population(
        id = "p",
        index_event = omop_index_event(
          201820L, "condition_occurrence", end_strategy = strategy
        )
      ),
      outputs = omop_output(type = "wide", population_id = "p")
    )
    exported <- recipe_export_circe(r, "p")
    expr <- jsonlite::fromJSON(exported, simplifyVector = FALSE)
    expect_equal(expr$EndStrategy, strategy)
    expect_equal(recipe_import_circe(exported)$index_event$end_strategy,
                 strategy)
  }
})

test_that("unsupported EndStrategy and CensoringCriteria fail closed", {
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
  expect_error(recipe_import_circe(circe), "DateField and Offset")

  unsupported <- jsonlite::fromJSON(circe, simplifyVector = FALSE)
  unsupported$EndStrategy <- list(CustomEra = list(DrugCodesetId = 1L))
  expect_error(recipe_import_circe(jsonlite::toJSON(
    unsupported, auto_unbox = TRUE, null = "null")), "EndStrategy")

  censored <- jsonlite::fromJSON(circe, simplifyVector = FALSE)
  censored$EndStrategy <- list()
  censored$CensoringCriteria <- list(list(Death = list()))
  expect_error(recipe_import_circe(jsonlite::toJSON(
    censored, auto_unbox = TRUE, null = "null")), "CensoringCriteria")
})

test_that("nested CriteriaGroups beyond one level fail closed", {
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
  expect_error(recipe_import_circe(circe), "nested CriteriaGroups")
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
  expect_false("has_concept" %in% types)
  expect_true("prior_observation" %in% types)
  expect_true("age_range" %in% types)

  expect_s3_class(pop$index_event, "omop_index_event")
  expect_equal(pop$index_event$concept_id, 201820L)
  expect_equal(pop$index_event$table, "condition_occurrence")
  expect_equal(.find_filter(pop$filters, "prior_observation")$params$min_days,
               365L)
  age <- .find_filter(pop$filters, "age_range")
  expect_equal(age$params$min, 18L)
  expect_equal(age$params$max, 65L)

  # Runnable: drop it into a recipe and compile a plan.
  r <- omop_recipe(populations = pop,
                   variables = omop_variable_age(),
                   outputs = omop_output(type = "wide", population_id = "ext"))
  expect_silent(dsOMOPClient:::recipe_to_plan(r))
})

test_that("recipe_export_circe errors for an unknown population_id", {
  expect_error(recipe_export_circe(omop_recipe(), population_id = "nope"),
               "not found")
})

test_that("typed index events compile and survive recipe round-trips", {
  physical_end <- list(DateOffset = list(
    DateField = "EndDate", Offset = 0L
  ))
  idx <- omop_index_event(201820L, "CONDITION_OCCURRENCE",
                          primary_limit = "LAST",
                          end_strategy = physical_end)
  expect_s3_class(idx, "omop_index_event")
  expect_equal(idx$table, "condition_occurrence")
  expect_equal(idx$primary_limit, "last")
  expect_error(omop_index_event(201820L, "death"), "must be one of")
  expect_error(omop_index_event(1.5, "condition_occurrence"),
               "non-negative integers")
  expect_error(omop_index_event(
    201820L, "condition_occurrence",
    end_strategy = list(DateOffset = list(DateField = "EndDate"))
  ), "DateField and Offset")
  expect_error(omop_index_event(
    201820L, "condition_occurrence",
    end_strategy = list(DateOffset = list(
      DateField = "EndDate", Offset = 0.5
    ))
  ), "exact integer")
  expect_error(omop_index_event(
    201820L, "condition_occurrence",
    end_strategy = list(DateOffset = list(
      DateField = "OtherDate", Offset = 0L
    ))
  ), "StartDate or EndDate")
  expect_error(omop_population("x", union = c("a", "b"), index_event = idx),
               "cannot also take")
  expect_error(omop_population("x", episode_policy = "any_episode",
                               index_event = idx), "cannot be combined")

  pop <- omop_population(
    "study", index_event = idx,
    filters = omop_filter_has_concept(
      255573L, "condition_occurrence", window = list(start = 0L, end = 30L)
    )
  )
  r <- omop_recipe(populations = pop,
                   variables = omop_variable_age(),
                   outputs = omop_output(type = "wide",
                                        population_id = "study"))
  plan <- recipe_to_plan(r)
  expect_equal(plan$populations$study$index_event,
               list(table = "condition_occurrence", concept_set = 201820L,
                    primary_limit = "last", end_strategy = physical_end))

  from_json <- recipe_import_json(recipe_export_json(r))
  from_yaml <- recipe_import_yaml(recipe_export_yaml(r))
  from_code <- eval(parse(text = recipe_to_code(r)))
  for (roundtrip in list(from_json, from_yaml, from_code)) {
    expect_s3_class(roundtrip$populations$study$index_event,
                    "omop_index_event")
    expect_equal(roundtrip$populations$study$index_event$primary_limit, "last")
    expect_equal(roundtrip$populations$study$index_event$end_strategy,
                 physical_end)
  }
})

test_that("Circe PrimaryCriteria First and Last stay explicit", {
  for (limit in c("first", "last")) {
    pop <- omop_population(
      "study",
      index_event = omop_index_event(
        201820L, "condition_occurrence", primary_limit = limit
      ),
      filters = omop_filter_has_concept(
        255573L, "condition_occurrence", window = list(start = 0L, end = 30L)
      )
    )
    r <- omop_recipe(populations = pop,
                     outputs = omop_output(type = "wide",
                                          population_id = "study"))
    json <- recipe_export_circe(r, "study")
    expr <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    circe_limit <- c(first = "First", last = "Last", all = "All")[[limit]]
    expect_equal(expr$PrimaryCriteria$PrimaryCriteriaLimit$Type, circe_limit)
    expect_equal(names(expr$PrimaryCriteria$CriteriaList[[1]]),
                 "ConditionOccurrence")

    back <- recipe_import_circe(json)
    expect_s3_class(back$index_event, "omop_index_event")
    expect_equal(back$index_event$primary_limit, limit)
    expect_equal(back$index_event$concept_id, 201820L)
    expect_false(any(vapply(back$filters, function(f) {
      identical(f$params$concept_id %||% NULL, 201820L)
    }, logical(1))))
    inclusion <- .find_filter(back$filters, "has_concept")
    expect_equal(inclusion$params$window,
                 list(start = 0L, end = 30L))
  }
})

test_that("Circe All fails closed until ERA collapse is executable", {
  pop <- omop_population(
    "study",
    index_event = omop_index_event(
      201820L, "condition_occurrence", primary_limit = "all"
    )
  )
  r <- omop_recipe(populations = pop,
                   outputs = omop_output(type = "wide",
                                        population_id = "study"))
  expect_error(recipe_export_circe(r, "study"), "ERA collapse")

  first <- .supported_recipe()
  expr <- jsonlite::fromJSON(recipe_export_circe(first, "t2d"),
                             simplifyVector = FALSE)
  expr$PrimaryCriteria$PrimaryCriteriaLimit$Type <- "All"
  expr$QualifiedLimit$Type <- "All"
  expr$ExpressionLimit$Type <- "All"
  all_json <- jsonlite::toJSON(expr, auto_unbox = TRUE, null = "null")
  expect_error(recipe_import_circe(all_json), "ERA collapse")
})

test_that("Circe count windows and supported Age operators are preserved", {
  pop <- omop_population(
    "study",
    index_event = omop_index_event(201820L, "condition_occurrence"),
    filters = omop_filter_concept_count(
      255573L, "condition_occurrence", min_count = 2L,
      window = list(start = -30L, end = 7L)
    )
  )
  r <- omop_recipe(populations = pop,
                   outputs = omop_output(type = "wide",
                                        population_id = "study"))
  back <- recipe_import_circe(recipe_export_circe(r, "study"))
  counted <- .find_filter(back$filters, "concept_count")
  expect_equal(counted$params$min_count, 2L)
  expect_equal(counted$params$window, list(start = -30L, end = 7L))

  age_expr <- function(op, value, extent = NULL) {
    age_demo <- list(Age = list(Value = value, Extent = extent, Op = op))
    age_rule <- list(
      name = "age",
      expression = list(
        Type = "ALL",
        CriteriaList = list(),
        DemographicCriteriaList = list(age_demo),
        Groups = list()
      )
    )
    expr <- list(
      ConceptSets = list(list(id = 0L, name = "index",
        expression = list(items = list(list(
          concept = list(CONCEPT_ID = 201820L), isExcluded = FALSE,
          includeDescendants = FALSE, includeMapped = FALSE))))),
      PrimaryCriteria = list(
        CriteriaList = list(list(ConditionOccurrence = list(CodesetId = 0L))),
        ObservationWindow = list(PriorDays = 0L, PostDays = 0L),
        PrimaryCriteriaLimit = list(Type = "First")),
      InclusionRules = list(age_rule),
      EndStrategy = list(), CensoringCriteria = list())
    jsonlite::toJSON(expr, auto_unbox = TRUE, null = "null")
  }
  expected <- list(gte = c(18L, 150L), gt = c(19L, 150L),
                   lte = c(0L, 65L), lt = c(0L, 64L),
                   eq = c(65L, 65L), bt = c(18L, 65L))
  for (op in names(expected)) {
    value <- if (op %in% c("lte", "lt", "eq")) 65L else 18L
    extent <- if (identical(op, "bt")) 65L else NULL
    imported <- recipe_import_circe(age_expr(op, value, extent))
    age <- .find_filter(imported$filters, "age_range")
    expect_equal(c(age$params$min, age$params$max), expected[[op]])
  }
  expect_error(recipe_import_circe(age_expr("!eq", 65L)),
               "unsupported Age operator")
})
