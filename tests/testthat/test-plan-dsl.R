test_that("ds.omop.plan creates empty plan", {
  plan <- ds.omop.plan()
  expect_s3_class(plan, "omop_plan")
  expect_null(plan$cohort)
  expect_equal(length(plan$outputs), 0)
  expect_false(plan$options$translate_concepts)
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
  plan <- ds.omop.plan.options(plan, translate_concepts = TRUE)
  expect_true(plan$options$translate_concepts)
})

test_that("plan.options sets block_sensitive", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.options(plan, block_sensitive = FALSE)
  expect_false(plan$options$block_sensitive)
})

test_that("plan.options sets min_persons", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.options(plan, min_persons = 5)
  expect_equal(plan$options$min_persons, 5)
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
  expect_true(3004410 %in% output$concept_set)
  expect_true(3025315 %in% output$concept_set)
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
    event_select = list(order = "first", n = 1),
    min_gap = 30
  )
  expect_s3_class(spec, "omop_temporal_spec")
  expect_equal(spec$index_window$start, -365)
  expect_equal(spec$index_window$end, 0)
  expect_equal(spec$calendar$start, "2020-01-01")
  expect_equal(spec$event_select$order, "first")
  expect_equal(spec$min_gap, 30)
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

test_that("omop.date_handling builds correct spec", {
  dh <- omop.date_handling(mode = "relative", reference = "index")
  expect_equal(dh$mode, "relative")
  expect_equal(dh$reference, "index")
  expect_null(dh$bin_width)

  dh2 <- omop.date_handling(mode = "binned", bin_width = "month")
  expect_equal(dh2$mode, "binned")
  expect_equal(dh2$bin_width, "month")
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
