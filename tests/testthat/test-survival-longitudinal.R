test_that("historical survival plan shape remains byte-compatible", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.survival(
    plan,
    outcome_table = "condition_occurrence",
    outcome_concepts = c(201820L, 316866L),
    tar = list(start_offset = 0L, end_offset = 365L),
    event_order = "last",
    name = "tte"
  )

  expect_named(
    plan$outputs$tte,
    c("type", "outcome", "tar", "event_order"),
    ignore.order = FALSE
  )
  expect_identical(plan$outputs$tte$event_order, "last")
  expect_null(plan$outputs$tte$outcomes)
  dependencies <- dsOMOPClient:::.plan_dependency_manifest(plan)$tables
  expect_true("observation_period" %in% names(dependencies))
  expect_false("death" %in% names(dependencies))
})

test_that("advanced survival builder retains named longitudinal semantics", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.survival(
    plan,
    outcomes = list(
      myocardial_infarction = list(
        table = "condition_occurrence", concept_set = 316866L
      ),
      revascularization = list(
        table = "procedure_occurrence", concept_set = 4301351L
      )
    ),
    tar = list(start_offset = 1L, end_offset = 730L),
    event_order = "all",
    format = "recurrent_events",
    censoring = list(
      cohort_end = TRUE, observation_period_end = TRUE, death = TRUE,
      admin_date = "2025-12-31"
    ),
    washout_days = 30L,
    tie_policy = "all",
    name = "recurrent"
  )

  output <- plan$outputs$recurrent
  expect_identical(output$format, "recurrent_events")
  expect_identical(output$event_order, "all")
  expect_identical(output$washout_days, 30L)
  expect_identical(names(output$outcomes),
                   c("myocardial_infarction", "revascularization"))
  expect_identical(output$censoring$admin_date, "2025-12-31")
  expect_identical(
    dsOMOPClient:::.plan_expected_output_symbols(
      plan, c(recurrent = "D_recurrent")
    )$recurrent,
    c("D_recurrent.events", "D_recurrent.riskSets")
  )
  dependencies <- dsOMOPClient:::.plan_dependency_manifest(plan)$tables
  expect_true(all(c(
    "condition_occurrence", "procedure_occurrence", "observation_period",
    "death"
  ) %in% names(dependencies)))
})

test_that("invalid longitudinal combinations fail on the client", {
  plan <- ds.omop.plan()
  endpoint <- list(mi = list(
    table = "condition_occurrence", concept_set = 316866L
  ))

  expect_error(
    ds.omop.plan.survival(
      plan, outcomes = endpoint, format = "survival", event_order = "all"
    ),
    "first or last"
  )
  expect_error(
    ds.omop.plan.survival(
      plan, outcomes = endpoint, format = "competing_risk",
      event_order = "last"
    ),
    "requires event_order='first'"
  )
  expect_error(
    ds.omop.plan.survival(
      plan, outcomes = endpoint, format = "counting_process",
      tie_policy = "all"
    ),
    "only for recurrent_events"
  )
  expect_error(
    ds.omop.plan.survival(
      plan, outcomes = endpoint,
      censoring = list(cohort_end = FALSE)
    ),
    "must remain TRUE"
  )
})

test_that("named recipe endpoints preserve table-specific filters", {
  recipe <- omop_recipe(
    variables = list(
      omop_variable(
        name = "MI outcome", table = "condition_occurrence",
        concept_id = 316866L, format = "binary",
        filters = omop_filter_date_range("2020-01-01", "2022-12-31")
      ),
      omop_variable(
        name = "PCI outcome", table = "procedure_occurrence",
        concept_id = 4301351L, format = "binary",
        filters = omop_filter_date_range("2021-01-01", "2023-12-31")
      )
    ),
    filters = omop_filter_date_range("2019-01-01", "2024-12-31"),
    outputs = omop_output(
      name = "events", type = "survival",
      options = list(
        format = "recurrent_events", outcome_mode = "named",
        event_order = "all", tie_policy = "all"
      )
    )
  )

  plan <- recipe_to_plan(recipe)
  endpoints <- plan$outputs$events$outcomes
  expect_identical(names(endpoints), c("mi_outcome", "pci_outcome"))
  expect_identical(
    unname(vapply(endpoints, `[[`, character(1), "table")),
    c("condition_occurrence", "procedure_occurrence")
  )
  expect_true(all(vapply(endpoints, function(endpoint) {
    is.list(endpoint$filters) && "and" %in% names(endpoint$filters)
  }, logical(1))))

  schema <- recipe_preview_schema(recipe)$events
  expect_identical(
    schema$column,
    c(
      "row_id", "cohort_row_id", "person_id", "outcome_name", "event",
      "event_number", "outcome_event_number", "event_days_from_index",
      "entry_days_from_index", "exit_days_from_index"
    )
  )
  expect_identical(attr(schema, "components")$risk_sets, c(
    "row_id", "cohort_row_id", "person_id", "entry_days_from_index",
    "exit_days_from_index", "follow_up_days"
  ))
})

test_that("composite recipes reject ambiguous multi-table outcomes", {
  recipe <- omop_recipe(
    variables = list(
      omop_variable(
        name = "mi", table = "condition_occurrence",
        concept_id = 316866L, format = "binary"
      ),
      omop_variable(
        name = "pci", table = "procedure_occurrence",
        concept_id = 4301351L, format = "binary"
      )
    ),
    outputs = omop_output(name = "tte", type = "survival")
  )

  expect_error(recipe_to_plan(recipe), "outcome_mode='named'")
})
