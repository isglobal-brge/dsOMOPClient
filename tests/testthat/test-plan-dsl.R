test_that("ds.omop.plan creates empty plan", {
  plan <- ds.omop.plan()
  expect_s3_class(plan, "omop_plan")
  expect_null(plan$cohort)
  expect_equal(length(plan$outputs), 0)
  expect_true(plan$options$translate_concepts)
  expect_true(plan$options$block_sensitive)
})

test_that("plan.cohort sets cohort by ID", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
  expect_equal(plan$cohort$type, "cohort_table")
  expect_equal(plan$cohort$cohort_definition_id, 1L)
})

test_that("plan.cohort sets cohort by spec", {
  plan <- ds.omop.plan()
  spec <- list(type = "condition", concept_set = c(201820))
  plan <- ds.omop.plan.cohort(plan, spec = spec)
  expect_equal(plan$cohort$type, "spec")
  expect_equal(plan$cohort$spec, spec)
})

test_that("plan.cohort requires one exact cohort source", {
  plan <- ds.omop.plan()
  spec <- list(type = "condition", concept_set = 201820L)

  expect_error(ds.omop.plan.cohort(plan), "Exactly one")
  expect_error(
    ds.omop.plan.cohort(plan, cohort_definition_id = 1, spec = spec),
    "Exactly one"
  )
  expect_error(ds.omop.plan.cohort(plan, cohort_definition_id = 1.5),
               "exact integer")
  expect_error(ds.omop.plan.cohort(plan, cohort_definition_id = c(1, 2)),
               "exact integer")
  expect_error(ds.omop.plan.cohort(plan, spec = list()), "non-empty")
  expect_error(ds.omop.plan.cohort(plan, spec = list("condition")),
               "named cohort")
  expect_error(ds.omop.plan.cohort(
    plan, spec = structure(list("condition", 201820L),
                           names = c("type", "type"))
  ), "named cohort")
})

test_that("plan harmonization rejects no-op modes and schema mismatches", {
  comparison <- list(
    servers = c("a", "b"),
    common_tables = c("person", "condition_occurrence"),
    common_columns = list(
      person = c("person_id", "gender_concept_id"),
      condition_occurrence = c("person_id", "condition_concept_id")
    ),
    column_diffs = list(
      person = list(a = "race_concept_id"),
      condition_occurrence = list(a = "condition_start_date")
    ),
    common_column_types = list(
      person = c(person_id = "numeric", gender_concept_id = "numeric"),
      condition_occurrence = c(
        person_id = "numeric", condition_concept_id = "numeric"
      )
    ),
    column_type_diffs = list(), column_errors = character(0)
  )
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) comparison
  )

  plan <- ds.omop.plan()
  plan <- ds.omop.plan.person_level(
    plan,
    tables = list(
      person = c("gender_concept_id", "race_concept_id"),
      death = "death_date"
    ),
    name = "demographics"
  )
  plan <- ds.omop.plan.events(
    plan, name = "conditions", table = "condition_occurrence",
    columns = c("person_id", "condition_start_date")
  )
  plan <- ds.omop.plan.options(plan, translate_concepts = FALSE)

  expect_error(ds.omop.plan.harmonize(plan, mode = "union_with_missing"),
               "not implemented")
  expect_error(ds.omop.plan.harmonize(plan, mode = "typo"),
               "intersection")
  expect_error(ds.omop.plan.harmonize(plan),
               "not executable with one common schema")

  expect_warning(
    trimmed <- ds.omop.plan.harmonize(plan, strict = FALSE),
    "removed incompatible inputs"
  )
  expect_identical(trimmed$outputs$demographics$tables,
                   list(person = "gender_concept_id"))
  expect_identical(trimmed$outputs$conditions$columns, "person_id")
})

test_that("non-strict harmonization rejects a plan with no remaining output", {
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) list(
      common_tables = "person", common_columns = list(),
      column_diffs = list()
    )
  )
  plan <- ds.omop.plan.events(
    ds.omop.plan(), name = "conditions", table = "condition_occurrence"
  )
  expect_error(
    suppressWarnings(ds.omop.plan.harmonize(plan, strict = FALSE)),
    "removed every output"
  )
})

test_that("plan harmonization fails closed on schema introspection errors", {
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) list(
      common_tables = "person",
      common_columns = list(person = character(0)),
      column_diffs = list(),
      column_errors = c(person = "metadata endpoint unavailable")
    )
  )
  plan <- ds.omop.plan.person_level(
    ds.omop.plan(), tables = list(person = "gender_concept_id")
  )
  expect_error(ds.omop.plan.harmonize(plan),
               "Cannot establish a common column contract")
  expect_error(ds.omop.plan.harmonize(plan, strict = FALSE),
               "Cannot establish a common column contract")
})

test_that("schema comparison exposes column introspection failures", {
  caps <- list(
    site_a = list(tables = "person"),
    site_b = list(tables = "person")
  )
  testthat::local_mocked_bindings(
    .get_session = function(symbol = "omop") list(capabilities = caps),
    ds.omop.columns = function(table, symbol = "omop", conns = NULL) {
      stop("metadata endpoint unavailable")
    }
  )
  comparison <- ds.omop.compare()
  expect_identical(comparison$common_tables, "person")
  expect_identical(comparison$common_columns$person, character(0))
  expect_match(comparison$column_errors[["person"]],
               "metadata endpoint unavailable")
})

test_that("schema comparison rejects partial aggregate responses", {
  caps <- list(
    site_a = list(tables = "person"),
    site_b = list(tables = "person")
  )
  partial <- list(
    site_a = data.frame(column_name = c("person_id", "year_of_birth"))
  )
  attr(partial, "ds_errors") <- list(site_b = "server unavailable")
  testthat::local_mocked_bindings(
    .get_session = function(symbol = "omop") list(capabilities = caps),
    ds.omop.columns = function(table, symbol = "omop", conns = NULL) partial
  )
  comparison <- ds.omop.compare()
  expect_identical(comparison$common_columns$person, character(0))
  expect_match(comparison$column_errors[["person"]],
               "site_b: server unavailable")
})

test_that("schema comparison canonicalizes compatible types and rejects drift", {
  caps <- list(
    site_a = list(tables = "measurement"),
    site_b = list(tables = "measurement")
  )
  metadata <- list(
    site_a = data.frame(
      column_name = c("person_id", "value_as_number"),
      cdm_datatype = c("integer", "float"),
      db_datatype = c("BIGINT", "FLOAT")
    ),
    site_b = data.frame(
      column_name = c("person_id", "value_as_number"),
      cdm_datatype = c("integer", "float"),
      db_datatype = c("INTEGER", "DOUBLE PRECISION")
    )
  )
  testthat::local_mocked_bindings(
    .get_session = function(symbol = "omop") list(capabilities = caps),
    ds.omop.columns = function(table, symbol = "omop", conns = NULL) metadata
  )
  compatible <- ds.omop.compare()
  expect_identical(compatible$servers, c("site_a", "site_b"))
  expect_identical(compatible$common_columns$measurement,
                   c("person_id", "value_as_number"))
  expect_identical(
    unname(compatible$common_column_types$measurement),
    c("numeric", "numeric")
  )
  expect_length(compatible$column_type_diffs, 0L)

  metadata$site_b$db_datatype[[2L]] <- "VARCHAR(100)"
  incompatible <- ds.omop.compare()
  expect_identical(incompatible$common_columns$measurement, "person_id")
  expect_identical(
    unname(incompatible$column_type_diffs$measurement$value_as_number),
    c("numeric", "cdm=numeric;db=character")
  )
})

test_that("schema comparison supports authorized extension column DB types", {
  caps <- list(
    site_a = list(tables = "measurement"),
    site_b = list(tables = "measurement")
  )
  metadata <- list(
    site_a = data.frame(column_name = "local_category", cdm_datatype = "",
                        db_datatype = "VARCHAR(20)"),
    site_b = data.frame(column_name = "local_category", cdm_datatype = "",
                        db_datatype = "TEXT")
  )
  testthat::local_mocked_bindings(
    .get_session = function(symbol = "omop") list(capabilities = caps),
    ds.omop.columns = function(table, symbol = "omop", conns = NULL) metadata
  )
  comparison <- ds.omop.compare()
  expect_identical(comparison$common_columns$measurement, "local_category")
  expect_identical(
    comparison$common_column_types$measurement[["local_category"]],
    "character"
  )
})

test_that("non-strict harmonization preserves aliases and feature structures", {
  comparison <- list(
    servers = c("a", "b"),
    common_tables = c("person", "measurement"),
    common_columns = list(
      person = c("person_id", "gender_concept_id"),
      measurement = c("person_id", "measurement_concept_id",
                      "value_as_number")
    ),
    common_column_types = list(
      person = c(person_id = "numeric", gender_concept_id = "numeric"),
      measurement = c(person_id = "numeric",
                      measurement_concept_id = "numeric",
                      value_as_number = "numeric")
    ),
    column_diffs = list(person = list(a = "race_concept_id")),
    column_type_diffs = list(), column_errors = character(0)
  )
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) comparison
  )
  plan <- ds.omop.plan.person_level(
    ds.omop.plan(),
    tables = list(person = c(sex = "gender_concept_id",
                             race = "race_concept_id")),
    name = "demographics"
  )
  plan <- ds.omop.plan.options(plan, translate_concepts = FALSE)
  feature_entry <- list(features = list(
    glucose = omop.feature.mean_value(3004410L)
  ))
  plan$outputs$features <- list(
    type = "person_level", tables = list(measurement = feature_entry)
  )
  expect_warning(
    harmonized <- ds.omop.plan.harmonize(plan, strict = FALSE),
    "removed incompatible inputs"
  )
  expect_identical(harmonized$outputs$demographics$tables$person,
                   list(sex = "gender_concept_id"))
  expect_identical(harmonized$outputs$features$tables$measurement,
                   feature_entry)
})

test_that("harmonization covers every public source-backed output type", {
  comparison <- list(
    servers = c("a", "b"), common_tables = character(0),
    common_columns = list(), common_column_types = list(),
    column_diffs = list(), column_type_diffs = list(),
    column_errors = character(0)
  )
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) comparison
  )
  cohort_plan <- function() ds.omop.plan.cohort(
    ds.omop.plan(), cohort_definition_id = 1L
  )
  plans <- list(
    baseline = ds.omop.plan.baseline(cohort_plan()),
    survival = ds.omop.plan.survival(
      cohort_plan(), outcome_concepts = 201820L
    ),
    concept_dictionary = ds.omop.plan.concept_dictionary(ds.omop.plan()),
    intervals_long = ds.omop.plan.intervals(
      cohort_plan(), tables = "visit_occurrence"
    ),
    temporal_covariates = ds.omop.plan.temporal_covariates(
      cohort_plan(), "condition_occurrence", 201820L
    ),
    person_period = ds.omop.plan.person_period(
      cohort_plan(), "condition_occurrence", 201820L
    )
  )
  for (name in names(plans)) {
    expect_error(
      ds.omop.plan.harmonize(plans[[name]]),
      "not executable with one common schema",
      info = name
    )
  }
  membership <- ds.omop.plan.cohort_membership(ds.omop.plan())
  expect_error(ds.omop.plan.harmonize(membership), "requires a cohort")
})

test_that("harmonization rejects unknown outputs and population drift", {
  comparison <- list(
    servers = c("a", "b"), common_tables = "person",
    common_columns = list(person = c("person_id", "year_of_birth")),
    common_column_types = list(
      person = c(person_id = "numeric", year_of_birth = "numeric")
    ),
    column_diffs = list(), column_type_diffs = list(),
    column_errors = character(0)
  )
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) comparison
  )
  bad <- ds.omop.plan()
  bad$outputs$x <- list(type = "typo")
  expect_error(ds.omop.plan.harmonize(bad), "Unsupported plan output type")

  scoped <- ds.omop.plan.cohort_membership(ds.omop.plan())
  scoped$populations <- list(base = list(
    filter_tree = list(type = "has_concept", params = list(
      table = "drug_exposure", concept_id = 1L
    ))
  ))
  expect_error(ds.omop.plan.harmonize(scoped, strict = FALSE),
               "population semantics")
})

test_that("harmonized plans are bound to plan and schema snapshots", {
  comparison <- list(
    servers = c("a", "b"), common_tables = "person",
    common_columns = list(person = c("gender_concept_id", "person_id")),
    common_column_types = list(
      person = c(gender_concept_id = "numeric", person_id = "numeric")
    ),
    column_diffs = list(), column_type_diffs = list(),
    column_errors = character(0)
  )
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) comparison
  )
  plan <- ds.omop.plan.person_level(
    ds.omop.plan(), tables = list(person = "gender_concept_id")
  )
  plan <- ds.omop.plan.options(plan, translate_concepts = FALSE)
  harmonized <- ds.omop.plan.harmonize(plan)
  expect_true(is.list(harmonized$harmonization$schema))
  expect_true(dsOMOPClient:::.verify_plan_schema_harmonization(
    harmonized, "omop", NULL
  ))

  changed <- harmonized
  changed$outputs$membership <- list(type = "cohort_membership")
  expect_error(
    dsOMOPClient:::.verify_plan_schema_harmonization(changed, "omop", NULL),
    "plan changed"
  )

  drifted <- comparison
  drifted$common_columns$person <- "person_id"
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) drifted
  )
  expect_error(
    dsOMOPClient:::.verify_plan_schema_harmonization(
      harmonized, "omop", NULL
    ),
    "schemas changed"
  )
})

test_that("dependency manifest captures cohort, longitudinal, and filter columns", {
  p <- ds.omop.plan.cohort(ds.omop.plan(), cohort_definition_id = 7L)
  p <- ds.omop.plan.baseline(p, derived = "age_at_index")
  p$populations <- list(base = list(
    kind = "criteria",
    filter_tree = list(type = "has_concept", params = list(
      table = "condition_occurrence", concept_id = 201820L,
      window = list(start = -365L, end = 0L)
    ))
  ))
  manifest <- dsOMOPClient:::.plan_dependency_manifest(p)

  expect_true(all(c("subject_id", "cohort_definition_id",
                    "cohort_start_date", "cohort_end_date") %in%
                  manifest$tables$cohort))
  expect_true(all(c("person_id", "observation_period_start_date",
                    "observation_period_end_date") %in%
                  manifest$tables$observation_period))
  expect_true(all(c("person_id", "condition_concept_id",
                    "condition_start_date") %in%
                  manifest$tables$condition_occurrence))
  expect_true("year_of_birth" %in% manifest$tables$person)
})

test_that("harmonization fails on implicit baseline and custom-filter dependencies", {
  comparison <- list(
    servers = c("a", "b"),
    common_tables = c("cohort", "person", "observation_period",
                      "condition_occurrence"),
    common_columns = list(
      cohort = c("subject_id", "cohort_definition_id", "cohort_start_date",
                 "cohort_end_date"),
      person = c("person_id", "gender_concept_id"),
      observation_period = c("person_id", "observation_period_start_date"),
      condition_occurrence = c("person_id", "condition_concept_id",
                               "condition_start_date")
    ),
    common_column_types = list(
      cohort = c(subject_id = "numeric", cohort_definition_id = "numeric",
                 cohort_start_date = "temporal", cohort_end_date = "temporal"),
      person = c(person_id = "numeric", gender_concept_id = "numeric"),
      observation_period = c(person_id = "numeric",
        observation_period_start_date = "temporal"),
      condition_occurrence = c(person_id = "numeric",
        condition_concept_id = "numeric", condition_start_date = "temporal")
    ),
    column_diffs = list(), column_type_diffs = list(),
    semantic_versions = list(), column_errors = character(0)
  )
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) comparison
  )
  baseline <- ds.omop.plan.cohort(ds.omop.plan(), cohort_definition_id = 1L)
  baseline <- ds.omop.plan.baseline(
    baseline, columns = "gender_concept_id", derived = character(0)
  )
  baseline <- ds.omop.plan.options(baseline, translate_concepts = FALSE)
  expect_error(ds.omop.plan.harmonize(baseline),
               "observation_period_end_date")

  survival <- ds.omop.plan.cohort(ds.omop.plan(), cohort_definition_id = 1L)
  survival <- ds.omop.plan.survival(survival, outcome_concepts = 201820L)
  survival$outputs$survival$filters <- list(custom = list(
    var = "local_outcome_flag", op = "eq", value = 1L
  ))
  survival <- ds.omop.plan.options(survival, translate_concepts = FALSE)
  expect_error(ds.omop.plan.harmonize(survival), "local_outcome_flag")
})

test_that("harmonization rejects dynamic wide/features and missing expansion tables", {
  dynamic_person <- ds.omop.plan()
  dynamic_person$outputs$person <- list(
    type = "person_level", tables = list(person = NULL)
  )
  expect_match(
    dsOMOPClient:::.plan_dependency_manifest(dynamic_person)$issues,
    "schema-dependent default columns"
  )
  expect_error(ds.omop.plan.harmonize(dynamic_person),
               "schema-dependent default columns")

  wide <- ds.omop.plan.events(
    ds.omop.plan(), "w", "condition_occurrence",
    concept_set = 201820L, representation = list(format = "wide")
  )
  expect_error(ds.omop.plan.harmonize(wide),
               "set translate_concepts = FALSE")

  auto <- ds.omop.plan.features(
    ds.omop.plan(), "f", "condition_occurrence", specs = list()
  )
  auto <- ds.omop.plan.options(auto, translate_concepts = FALSE)
  expect_error(ds.omop.plan.harmonize(auto), "automatic features")

  expanded <- ds.omop.plan.events(
    ds.omop.plan(), "e", "condition_occurrence",
    concept_set = list(concepts = 201820L, include_descendants = TRUE)
  )
  expanded <- ds.omop.plan.options(expanded, translate_concepts = FALSE)
  comparison <- list(
    servers = c("a", "b"), common_tables = "condition_occurrence",
    common_columns = list(condition_occurrence = c(
      "person_id", "condition_concept_id"
    )),
    common_column_types = list(condition_occurrence = c(
      person_id = "numeric", condition_concept_id = "numeric"
    )),
    column_diffs = list(), column_type_diffs = list(),
    semantic_versions = list(), column_errors = character(0)
  )
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) comparison
  )
  expect_error(ds.omop.plan.harmonize(expanded), "concept_ancestor")
})

test_that("vocabulary-dependent plans require one reported vocabulary version", {
  comparison <- list(
    servers = c("a", "b"),
    common_tables = c("condition_occurrence", "concept"),
    common_columns = list(
      condition_occurrence = c("person_id", "condition_concept_id"),
      concept = c("concept_id", "concept_name")
    ),
    common_column_types = list(
      condition_occurrence = c(person_id = "numeric",
                               condition_concept_id = "numeric"),
      concept = c(concept_id = "numeric", concept_name = "character")
    ),
    column_diffs = list(), column_type_diffs = list(),
    semantic_versions = list(
      a = list(cdm_version = "5.4", spec_version = "5.4",
               vocabulary_version = "2025-01"),
      b = list(cdm_version = "5.4", spec_version = "5.4",
               vocabulary_version = "2025-06")
    ),
    column_errors = character(0)
  )
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) comparison
  )
  p <- ds.omop.plan.events(
    ds.omop.plan(), "events", "condition_occurrence",
    concept_set = 201820L
  )
  expect_error(ds.omop.plan.harmonize(p), "vocabulary version")
})

test_that("schema binding ignores irrelevant columns but detects required drift", {
  comparison <- list(
    servers = c("a", "b"), common_tables = "person",
    common_columns = list(person = c(
      "person_id", "gender_concept_id", "local_note"
    )),
    common_column_types = list(person = c(
      person_id = "numeric", gender_concept_id = "numeric",
      local_note = "character"
    )),
    column_diffs = list(), column_type_diffs = list(),
    semantic_versions = list(), column_errors = character(0)
  )
  current <- comparison
  testthat::local_mocked_bindings(
    ds.omop.compare = function(symbol = "omop", conns = NULL,
                               tables = NULL) current
  )
  p <- ds.omop.plan.person_level(
    ds.omop.plan(), list(person = "gender_concept_id")
  )
  p <- ds.omop.plan.options(p, translate_concepts = FALSE)
  bound <- ds.omop.plan.harmonize(p)
  current$common_columns$person <- c(
    "person_id", "gender_concept_id", "another_extension"
  )
  current$common_column_types$person <- c(
    person_id = "numeric", gender_concept_id = "numeric",
    another_extension = "numeric"
  )
  expect_true(dsOMOPClient:::.verify_plan_schema_harmonization(
    bound, "omop", NULL
  ))
  current$common_columns$person <- "person_id"
  current$common_column_types$person <- c(person_id = "numeric")
  expect_error(dsOMOPClient:::.verify_plan_schema_harmonization(
    bound, "omop", NULL
  ), "schemas changed")
})

test_that("plan.baseline adds baseline output with derived fields", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.baseline(plan,
    columns = c("gender_concept_id", "year_of_birth"),
    derived = c("age_at_index", "prior_observation"),
    name = "demo"
  )
  expect_true("demo" %in% names(plan$outputs))
  expect_equal(plan$outputs$demo$type, "baseline")
  expect_equal(plan$outputs$demo$columns, c("gender_concept_id", "year_of_birth"))
  expect_equal(plan$outputs$demo$derived, c("age_at_index", "prior_observation"))
})

test_that("plan.person_level adds person-level output", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.person_level(plan,
    tables = list(person = c("person_id", "gender_concept_id")),
    name = "demo"
  )
  expect_true("demo" %in% names(plan$outputs))
  expect_equal(plan$outputs$demo$type, "person_level")
  expect_equal(length(plan$outputs$demo$tables), 1)
})

test_that("plan.events adds event-level output", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.events(plan,
    name = "conditions",
    table = "condition_occurrence",
    concept_set = c(201820, 255573),
    columns = c("condition_start_date"),
    representation = list(format = "long")
  )
  expect_true("conditions" %in% names(plan$outputs))
  expect_equal(plan$outputs$conditions$type, "event_level")
  expect_equal(plan$outputs$conditions$table, "condition_occurrence")
  expect_equal(plan$outputs$conditions$concept_set, c(201820, 255573))
})

test_that("plan.events with time window", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.events(plan,
    name = "recent",
    table = "measurement",
    time_window = list(start_date = "2020-01-01", end_date = "2023-12-31")
  )
  expect_equal(plan$outputs$recent$filters$time_window$start_date, "2020-01-01")
})

test_that("plan.outcome adds outcome extraction", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.outcome(plan,
    name = "diabetes",
    concept_set = c(201820)
  )
  expect_true("diabetes" %in% names(plan$outputs))
  expect_equal(plan$outputs$diabetes$table, "condition_occurrence")
  expect_equal(plan$outputs$diabetes$representation$format, "features")
})

test_that("plan.options sets translate_concepts", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.options(plan, translate_concepts = FALSE)
  expect_false(plan$options$translate_concepts)
})

test_that("plan.options sets block_sensitive", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.options(plan, block_sensitive = FALSE)
  expect_false(plan$options$block_sensitive)
})

test_that("plan.features adds feature specs", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.features(plan,
    name = "clinical",
    table = "measurement",
    specs = list(
      hba1c = omop.feature.boolean(c(3004410)),
      weight = omop.feature.count(c(3025315))
    )
  )
  expect_true("clinical" %in% names(plan$outputs))
  output <- plan$outputs$clinical
  expect_equal(output$representation$format, "features")
  expect_null(output$concept_set)
  expect_null(output$filters$concept_set)
  expect_equal(output$representation$features$hba1c$concept_set, 3004410)
  expect_equal(output$representation$features$weight$concept_set, 3025315)

  expect_error(ds.omop.plan.features(
    ds.omop.plan(), "bad", "measurement",
    specs = list(x = omop.feature.count(1.5))),
    "feature concept_set.*exact integers")

  episode <- ds.omop.plan.features(
    ds.omop.plan(), "episode_features", "measurement",
    specs = list(n = omop.feature.count(3004410L)),
    grain = "episode",
    temporal = omop.temporal(index_window = list(start = -30L, end = 0L))
  )$outputs$episode_features
  expect_equal(episode$representation$grain, "episode")
  expect_equal(episode$temporal$index_window,
               list(start = -30L, end = 0L))
  expect_error(
    ds.omop.plan.features(
      ds.omop.plan(), "bad_episode", "measurement",
      specs = list(n = omop.feature.count(3004410L)), grain = "episode"
    ),
    "require temporal\\$index_window"
  )
})

test_that("plan temporal offsets are exact integers and ordered", {
  expect_error(
    ds.omop.plan.temporal_covariates(
      ds.omop.plan(), "measurement", 1, bin_width = 2.5),
    "bin_width.*exact integer")
  expect_error(
    ds.omop.plan.temporal_covariates(
      ds.omop.plan(), "measurement", 1, bin_width = 0),
    "bin_width must be >= 1")
  expect_error(
    ds.omop.plan.temporal_covariates(
      ds.omop.plan(), "measurement", 1,
      window_start = 1, window_end = 0),
    "not be after window_end")
  expect_error(omop.temporal(index_window = list(start = -2.5, end = 0)),
               "index_window\\$start.*exact integer")
  expect_error(omop.temporal(event_select = list(order = "first", n = 1.5)),
               "event_select\\$n.*exact integer")
  expect_error(ds.omop.plan.survival(
    ds.omop.plan(), outcome_concepts = 1,
    tar = list(start_offset = 0.5, end_offset = 30)),
    "start_offset.*exact integer")

  tc <- ds.omop.plan.temporal_covariates(
    ds.omop.plan(), "measurement", 1,
    bin_width = 14, window_start = -30, window_end = 0)$outputs$temporal
  expect_identical(tc$bin_width, 14L)
  expect_identical(tc$window_start, -30L)

  one_day <- ds.omop.plan.temporal_covariates(
    ds.omop.plan(), "measurement", 1,
    bin_width = 14, window_start = 0, window_end = 0)$outputs$temporal
  expect_identical(one_day$window_start, 0L)
  expect_identical(one_day$window_end, 0L)
})

test_that("multiple outputs can be added to a plan", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.baseline(plan, name = "demo")
  plan <- ds.omop.plan.events(plan,
    name = "meds", table = "drug_exposure",
    concept_set = c(1124300))
  plan <- ds.omop.plan.outcome(plan,
    name = "outcome", concept_set = c(201820))

  expect_equal(length(plan$outputs), 3)
  expect_true(all(c("demo", "meds", "outcome") %in% names(plan$outputs)))
})

test_that(".ds_encode preserves names of a multi-element out-mapping", {
  # Regression: a named character vector (the execute out-mapping with >1
  # entry) used to serialize to a JSON array, dropping its names, so the
  # server could not match plan outputs to symbols and assigned nothing.
  # Decode exactly as the server's .ds_arg does.
  ds_arg_decode <- function(x) {
    b64 <- substring(x, 5)
    b64 <- gsub("-", "+", b64)
    b64 <- gsub("_", "/", b64)
    pad <- (4 - nchar(b64) %% 4) %% 4
    if (pad > 0) b64 <- paste0(b64, strrep("=", pad))
    jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)),
                       simplifyVector = FALSE)
  }

  out <- c(demo = "D", drugs = "X", outcome = "Y")
  decoded <- ds_arg_decode(dsOMOPClient:::.ds_encode(out))
  expect_equal(sort(names(decoded)), c("demo", "drugs", "outcome"))
  expect_equal(decoded[["demo"]], "D")
  expect_equal(decoded[["drugs"]], "X")
  expect_equal(decoded[["outcome"]], "Y")

  # Unnamed vectors (e.g. concept-id sets) must remain JSON arrays.
  ids <- dsOMOPClient:::.ds_encode(c(19059056L, 19078461L))
  decoded_ids <- ds_arg_decode(ids)
  expect_null(names(decoded_ids))
  expect_equal(as.integer(unlist(decoded_ids)), c(19059056L, 19078461L))
})

test_that("print.omop_plan works", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
  plan <- ds.omop.plan.baseline(plan, name = "demo")
  plan <- ds.omop.plan.events(plan,
    name = "conditions", table = "condition_occurrence",
    concept_set = c(201820))

  output <- capture.output(print(plan))
  expect_true(any(grepl("Extraction Plan", output)))
  expect_true(any(grepl("Cohort", output)))
  expect_true(any(grepl("Outputs", output)))
})

# --- Temporal and date handling helper tests ---

test_that("omop.temporal builds correct spec", {
  spec <- omop.temporal(
    index_window = list(start = -365, end = 0),
    calendar = list(start = "2020-01-01", end = "2023-12-31"),
    event_select = list(order = "first", n = 1)
  )
  expect_s3_class(spec, "omop_temporal_spec")
  expect_equal(spec$index_window$start, -365)
  expect_equal(spec$index_window$end, 0)
  expect_equal(spec$calendar$start, "2020-01-01")
  expect_equal(spec$event_select$order, "first")
  expect_equal(spec$event_select$by, "grain")
  expect_null(spec$min_gap)

  gap <- omop.temporal(min_gap = 30)
  expect_identical(gap$min_gap,
                   list(days = 30L, by = "concept", keep = "first"))
  gap_last <- omop.temporal(
    min_gap = list(days = 7, by = "grain", keep = "last")
  )
  expect_identical(gap_last$min_gap,
                   list(days = 7L, by = "grain", keep = "last"))
  expect_error(omop.temporal(min_gap = 0), "min_gap\\$days must be >= 1")
  expect_error(omop.temporal(min_gap = list(days = 2, by = "visit")), "arg")
  expect_error(omop.temporal(
    min_gap = list(days = 2, strategy = "first")
  ), "days/by/keep")

  per_concept <- omop.temporal(
    event_select = list(order = "last", n = 2L, by = "concept")
  )
  expect_equal(per_concept$event_select$by, "concept")
  expect_error(
    omop.temporal(event_select = list(
      order = "first", n = 1L, by = "visit"
    )),
    "arg"
  )
})

test_that("events support episode-grain sparse output", {
  temporal <- omop.temporal(index_window = list(start = -30L, end = 0L))
  output <- ds.omop.plan.events(
    ds.omop.plan(), "sparse_episodes", "condition_occurrence",
    temporal = temporal,
    representation = list(format = "sparse", grain = "episode")
  )$outputs$sparse_episodes
  expect_equal(output$representation$grain, "episode")
  expect_equal(output$temporal$index_window, list(start = -30L, end = 0L))
  expect_error(
    ds.omop.plan.events(
      ds.omop.plan(), "bad_sparse", "condition_occurrence",
      temporal = temporal, representation = list(format = "sparse")
    ),
    "grain='episode'"
  )
})

test_that("omop.temporal with partial spec", {
  spec <- omop.temporal(
    index_window = list(start = -30, end = 0)
  )
  expect_s3_class(spec, "omop_temporal_spec")
  expect_true(!is.null(spec$index_window))
  expect_null(spec$calendar)
  expect_null(spec$event_select)
})

test_that("omop.temporal validates calendar dates without hardcoding privacy width", {
  one_sided <- omop.temporal(calendar = list(start = "2024-01-01"))
  expect_equal(one_sided$calendar$start, "2024-01-01")

  expect_error(
    omop.temporal(calendar = list(start = "2024-1-01")),
    "ISO date"
  )
  expect_error(
    omop.temporal(calendar = list(start = "2024-02-30")),
    "valid calendar date"
  )
  expect_error(
    omop.temporal(calendar = list(
      start = "2024-02-01", end = "2024-01-01"
    )),
    "start must not be after end"
  )
})

test_that("omop.date_handling builds correct spec", {
  secure_default <- omop.date_handling()
  expect_equal(secure_default$mode, "remove")

  dh <- omop.date_handling(mode = "relative", reference = "index")
  expect_equal(dh$mode, "relative")
  expect_equal(dh$reference, "index")
  expect_null(dh$bin_width)

  dh2 <- omop.date_handling(mode = "binned", bin_width = "month")
  expect_equal(dh2$mode, "binned")
  expect_equal(dh2$bin_width, "month")

  expect_error(omop.date_handling(mode = "binned"), "bin_width is required")
  expect_error(omop.date_handling(mode = "relative", bin_width = "month"),
               "only valid")
  expect_error(omop.date_handling(mode = "unknown"), "arg")
  expect_error(omop.date_handling(reference = "today"), "arg")
  expect_error(omop.date_handling(date_columns = c("ok_date", "bad;drop")),
               "column identifiers")
  expect_equal(omop.date_handling(date_columns = c("START_DATE", "start_date"))
               $date_columns, "start_date")
})

test_that("YAML plan loading never evaluates !expr tags", {
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines('!expr "options(dsomop_yaml_plan_executed = TRUE)"', path)
  withr::local_options(list(
    yaml.eval.expr = TRUE,
    dsomop_yaml_plan_executed = NULL
  ))
  expect_error(ds.omop.plan.load(path))
  expect_null(getOption("dsomop_yaml_plan_executed"))
})

test_that("ds.omop.plan.events stores temporal and date_handling", {
  plan <- ds.omop.plan()
  temp <- omop.temporal(
    index_window = list(start = -365, end = 0)
  )
  dh <- omop.date_handling(mode = "relative")

  plan <- ds.omop.plan.events(plan,
    name = "conditions",
    table = "condition_occurrence",
    temporal = temp,
    date_handling = dh
  )

  out <- plan$outputs$conditions
  expect_true(!is.null(out$temporal))
  expect_equal(out$temporal$index_window$start, -365)
  expect_true(!is.null(out$date_handling))
  expect_equal(out$date_handling$mode, "relative")
})

test_that("plan.survival adds survival output", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.survival(plan,
    outcome_table = "condition_occurrence",
    outcome_concepts = c(4000002),
    tar = list(start_offset = 0, end_offset = 730),
    event_order = "first",
    name = "tte"
  )
  expect_true("tte" %in% names(plan$outputs))
  expect_equal(plan$outputs$tte$type, "survival")
  expect_equal(plan$outputs$tte$outcome$table, "condition_occurrence")
  expect_equal(plan$outputs$tte$outcome$concept_set, 4000002L)
  expect_equal(plan$outputs$tte$tar$end_offset, 730)
  expect_equal(plan$outputs$tte$event_order, "first")
})

test_that("plan.concept_dictionary adds dictionary output", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.concept_dictionary(plan,
    source_outputs = c("events", "baseline"),
    name = "dict"
  )
  expect_true("dict" %in% names(plan$outputs))
  expect_equal(plan$outputs$dict$type, "concept_dictionary")
  expect_equal(plan$outputs$dict$source_outputs, c("events", "baseline"))
})

test_that("plan.concept_dictionary defaults to NULL source_outputs", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.concept_dictionary(plan)
  expect_true("concept_dictionary" %in% names(plan$outputs))
  expect_null(plan$outputs$concept_dictionary$source_outputs)
})

test_that("print.omop_plan shows new output types", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
  plan <- ds.omop.plan.baseline(plan, name = "demo")
  plan <- ds.omop.plan.survival(plan,
    outcome_concepts = c(4000002), name = "tte")
  plan <- ds.omop.plan.concept_dictionary(plan, name = "dict")

  output <- capture.output(print(plan))
  expect_true(any(grepl("baseline", output)))
  expect_true(any(grepl("survival", output)))
  expect_true(any(grepl("dictionary", output)))
})

test_that("plan.cohort_membership adds cohort membership output", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.cohort_membership(plan, name = "cm")
  expect_true("cm" %in% names(plan$outputs))
  expect_equal(plan$outputs$cm$type, "cohort_membership")
})

test_that("plan.intervals adds intervals_long output", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.intervals(plan,
    tables = c("condition_occurrence", "drug_exposure"),
    concept_filter = list(condition_occurrence = c(201820)),
    name = "iv"
  )
  expect_true("iv" %in% names(plan$outputs))
  expect_equal(plan$outputs$iv$type, "intervals_long")
  expect_equal(plan$outputs$iv$tables,
               c("condition_occurrence", "drug_exposure"))
  expect_equal(plan$outputs$iv$concept_filter$condition_occurrence,
               c(201820))
})

test_that("plan.intervals normalizes table keys and preserves OHDSI concept sets", {
  plan <- ds.omop.plan.intervals(
    ds.omop.plan(),
    tables = "Condition_Occurrence",
    concept_filter = list(Condition_Occurrence = list(
      concepts = 201820,
      include_descendants = TRUE,
      exclude = integer(0)
    )),
    filters = list(Condition_Occurrence = list(
      var = "condition_type_concept_id", op = "in", value = 32020
    ))
  )

  expect_identical(plan$outputs$intervals$tables, "condition_occurrence")
  expect_named(plan$outputs$intervals$concept_filter, "condition_occurrence")
  expect_true(plan$outputs$intervals$concept_filter[[1]]$include_descendants)
  expect_named(plan$outputs$intervals$source_filters, "condition_occurrence")
})

test_that("longitudinal builders preserve OHDSI and dynamic concept scope", {
  dynamic <- ds.omop.plan.temporal_covariates(
    ds.omop.plan(), table = "condition_occurrence", concept_set = NULL
  )
  expect_null(dynamic$outputs$temporal$concept_set)

  concept_spec <- list(
    concepts = c(201820L, 201826L),
    include_descendants = TRUE,
    include_mapped = FALSE,
    exclude = 999999L
  )
  temporal <- ds.omop.plan.temporal_covariates(
    ds.omop.plan(), table = "condition_occurrence",
    concept_set = concept_spec
  )
  expect_identical(temporal$outputs$temporal$concept_set, concept_spec)

  survival <- ds.omop.plan.survival(
    ds.omop.plan(), outcome_concepts = concept_spec
  )
  expect_identical(
    survival$outputs$survival$outcome$concept_set, concept_spec
  )
})

test_that("plan.intervals has default tables", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.intervals(plan)
  expect_equal(plan$outputs$intervals$tables,
               c("observation_period", "visit_occurrence",
                 "drug_exposure", "condition_occurrence"))
})

test_that("plan.temporal_covariates adds temporal output", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.temporal_covariates(plan,
    table = "condition_occurrence",
    concept_set = c(201820, 255573),
    bin_width = 30L,
    window_start = -365L,
    window_end = 0L,
    analyses = c("binary", "count"),
    name = "tc"
  )
  expect_true("tc" %in% names(plan$outputs))
  expect_equal(plan$outputs$tc$type, "temporal_covariates")
  expect_equal(plan$outputs$tc$table, "condition_occurrence")
  expect_equal(plan$outputs$tc$concept_set, c(201820L, 255573L))
  expect_equal(plan$outputs$tc$bin_width, 30L)
  expect_equal(plan$outputs$tc$window_start, -365L)
  expect_equal(plan$outputs$tc$window_end, 0L)
  expect_equal(plan$outputs$tc$analyses, c("binary", "count"))
})

test_that("plan.temporal_covariates has defaults", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.temporal_covariates(plan,
    table = "condition_occurrence",
    concept_set = c(201820)
  )
  out <- plan$outputs$temporal
  expect_equal(out$bin_width, 30L)
  expect_equal(out$window_start, -365L)
  expect_equal(out$window_end, 0L)
  expect_equal(out$analyses, c("binary"))
})

test_that("print.omop_plan shows new output types A/E/G", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
  plan <- ds.omop.plan.cohort_membership(plan, name = "cm")
  plan <- ds.omop.plan.intervals(plan, name = "iv")
  plan <- ds.omop.plan.temporal_covariates(plan,
    table = "condition_occurrence",
    concept_set = c(201820), name = "tc")

  output <- capture.output(print(plan))
  expect_true(any(grepl("cohort", output)))
  expect_true(any(grepl("intervals", output)))
  expect_true(any(grepl("temporal", output)))
})

test_that("print.omop_plan shows temporal info", {
  plan <- ds.omop.plan()
  temp <- omop.temporal(
    index_window = list(start = -365, end = 0),
    event_select = list(order = "first", n = 1)
  )
  plan <- ds.omop.plan.events(plan,
    name = "conditions",
    table = "condition_occurrence",
    concept_set = c(201820),
    temporal = temp,
    date_handling = omop.date_handling(mode = "relative")
  )

  output <- capture.output(print(plan))
  expect_true(any(grepl("index-window", output)))
  expect_true(any(grepl("dates:relative", output)))
})
