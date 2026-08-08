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
      plan, outcomes = endpoint, format = "competing_risk",
      tie_policy = "error"
    ),
    "disclosure oracle"
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

test_that("multi-state plans retain a canonical cyclic transition graph", {
  transition_matrix <- matrix(
    NA_integer_, nrow = 3L, ncol = 3L,
    dimnames = list(
      c("index", "mi", "recovery"),
      c("index", "mi", "recovery")
    )
  )
  transition_matrix["index", "mi"] <- 1L
  transition_matrix["mi", "recovery"] <- 2L
  transition_matrix["recovery", "mi"] <- 3L

  plan <- ds.omop.plan.survival(
    ds.omop.plan(),
    outcomes = list(
      mi = list(
        table = "condition_occurrence", concept_set = 316866L
      ),
      recovery = list(
        table = "observation", concept_set = 4207907L
      )
    ),
    format = "multi_state",
    transitions = transition_matrix,
    initial_state = "index",
    state_hierarchy = c("mi", "recovery", "index"),
    tie_policy = "sequential",
    name = "disease_course"
  )

  output <- plan$outputs$disease_course
  expect_identical(output$event_order, "all")
  expect_identical(output$initial_state, "index")
  expect_identical(output$transitions$states, c("index", "mi", "recovery"))
  expect_identical(
    vapply(output$transitions$edges, `[[`, integer(1L), "trans"),
    1:3
  )
  expect_identical(output$state_hierarchy, c("mi", "recovery", "index"))
  expect_equal(output$state_step, 0.01)
  expect_identical(
    dsOMOPClient:::.plan_expected_output_symbols(
      plan, c(disease_course = "D_course")
    )$disease_course,
    c("D_course.msdata", "D_course.transitionRef")
  )

  reversible <- ds.omop.plan.survival(
    ds.omop.plan(),
    outcomes = list(
      well = list(table = "observation", concept_set = 10L),
      ill = list(table = "condition_occurrence", concept_set = 20L)
    ),
    format = "multi_state",
    transitions = list(well = "ill", ill = "well"),
    initial_state = "well"
  )
  expect_identical(reversible$outputs$survival$initial_state, "well")
  expect_identical(reversible$outputs$survival$transitions$states,
                   c("well", "ill"))
})

test_that("multi-state client validation rejects ambiguous graph semantics", {
  endpoint <- list(mi = list(
    table = "condition_occurrence", concept_set = 316866L
  ))
  graph <- list(index = "mi", mi = character(0))

  expect_error(
    ds.omop.plan.survival(
      ds.omop.plan(), outcomes = endpoint, format = "multi_state",
      transitions = graph, event_order = "first"
    ),
    "event_order='all'"
  )
  expect_error(
    ds.omop.plan.survival(
      ds.omop.plan(), outcomes = endpoint, format = "counting_process",
      tie_policy = "sequential"
    ),
    "only for multi_state"
  )
  expect_error(
    ds.omop.plan.survival(
      ds.omop.plan(), outcomes = endpoint, format = "multi_state",
      transitions = list(index = character(0), mi = character(0))
    ),
    "at least one transition"
  )
  duplicate_edge <- list(from = "index", to = "mi", 1L, 2L)
  names(duplicate_edge)[3:4] <- "trans"
  expect_error(
    ds.omop.plan.survival(
      ds.omop.plan(), outcomes = endpoint, format = "multi_state",
      transitions = list(
        states = c("index", "mi"), edges = list(duplicate_edge)
      )
    ),
    "must contain from, to and trans"
  )

  imported_extra <- list(transitions = list(
    states = c("index", "mi"),
    edges = list(list(from = "index", to = "mi", trans = 1L,
                      private_hint = "drop-me"))
  ))
  expect_error(
    dsOMOPClient:::.recipe_restore_multistate_options(imported_extra),
    "contain exactly from, to and trans"
  )
  imported_vector <- list(transitions = list(
    states = c("index", "mi"),
    edges = list(list(from = c("index", "mi"), to = "mi", trans = 1L))
  ))
  expect_error(
    dsOMOPClient:::.recipe_restore_multistate_options(imported_vector),
    "one from and one to state"
  )
})

test_that("multi-state recipes round-trip and preview both components", {
  transition_matrix <- matrix(
    NA_integer_, nrow = 3L, ncol = 3L,
    dimnames = list(
      c("index", "mi", "recovery"),
      c("index", "mi", "recovery")
    )
  )
  transition_matrix["index", "mi"] <- 1L
  transition_matrix["mi", "recovery"] <- 2L
  transition_matrix["recovery", "mi"] <- 3L
  recipe <- omop_recipe(
    variables = list(
      omop_variable(
        name = "mi", table = "condition_occurrence",
        concept_id = 316866L, format = "binary"
      ),
      omop_variable(
        name = "recovery", table = "observation",
        concept_id = 4207907L, format = "binary"
      )
    ),
    outputs = omop_output(
      name = "course", type = "survival",
      options = list(
        format = "multi_state",
        transitions = transition_matrix, initial_state = "index",
        state_hierarchy = c("mi", "recovery", "index"),
        tie_policy = "sequential"
      )
    )
  )

  plan <- recipe_to_plan(recipe)
  expect_identical(plan$outputs$course$format, "multi_state")
  expect_identical(plan$outputs$course$event_order, "all")
  expect_identical(
    recipe_to_plan(recipe_import_json(recipe_export_json(recipe)))$outputs$course,
    plan$outputs$course
  )
  expect_identical(
    recipe_to_plan(recipe_import_yaml(recipe_export_yaml(recipe)))$outputs$course,
    plan$outputs$course
  )
  expect_identical(
    recipe_to_plan(eval(parse(text = recipe_to_code(recipe))))$outputs$course,
    plan$outputs$course
  )

  schema <- recipe_preview_schema(recipe)$course
  expect_identical(schema$column, c(
    "row_id", "cohort_row_id", "person_id", "from", "to", "trans",
    "Tstart", "Tstop", "time", "status", "from_name", "to_name",
    "state_visit_number"
  ))
  expect_identical(attr(schema, "components")$msdata, schema$column)
  expect_true(all(c(
    "from", "to", "trans", "from_name", "to_name", "tie_policy"
  ) %in% attr(schema, "components")$transition_ref))

  invalid <- recipe
  invalid$outputs$course$options$outcome_mode <- "composite"
  expect_error(recipe_to_plan(invalid), "requires outcome_mode='named'")
})
