test_that("person-period plan declares an explicit regular episode panel", {
  plan <- ds.omop.plan.person_period(
    ds.omop.plan(),
    table = "condition_occurrence",
    concept_set = c(201820, 255573),
    bin_width = 14,
    window_start = -28,
    window_end = 0,
    analyses = c("binary", "count"),
    name = "panel"
  )

  out <- plan$outputs$panel
  expect_identical(out$type, "person_period")
  expect_identical(out$grain, "episode")
  expect_identical(out$time_origin, "index")
  expect_identical(out$bin_width, 14L)
  expect_identical(out$window_start, -28L)
  expect_identical(out$window_end, 0L)
  expect_identical(out$concept_set, c(201820L, 255573L))

  expect_error(ds.omop.plan.person_period(
    ds.omop.plan(), "condition_occurrence", 201820, grain = "person"
  ), "grain.*episode")
  expect_error(ds.omop.plan.person_period(
    ds.omop.plan(), "condition_occurrence", 201820,
    time_origin = "calendar"
  ), "time_origin.*index")
})

test_that("person-period recipes compile without inventing longitudinal fields", {
  recipe <- omop_recipe(
    variables = list(
      omop_variable(
        name = "dx_present", table = "condition_occurrence",
        concept_id = 201820, format = "binary"
      ),
      omop_variable(
        name = "dx_count", table = "condition_occurrence",
        concept_id = 201820, format = "count"
      )
    ),
    outputs = omop_output(
      name = "panel", type = "person_period",
      options = list(
        bin_width = 7L, window_start = -14L, window_end = 0L,
        analyses = c("binary", "count")
      )
    )
  )

  out <- recipe_to_plan(recipe)$outputs$panel
  expect_identical(out$type, "person_period")
  expect_identical(out$grain, "episode")
  expect_identical(out$time_origin, "index")
  expect_identical(out$bin_width, 7L)
  expect_identical(out$analyses, c("binary", "count"))

  schema <- recipe_preview_schema(recipe)$panel
  expect_true(all(paste0("personPeriods.",
                         c("rowId", "timeId", "startDay", "endDay")) %in%
                    schema$column))
  expect_equal(attr(schema, "join_key"), "rowId")

  recipe$outputs$panel$options$grain <- "person"
  expect_error(recipe_to_plan(recipe), "unsupported option.*grain")
})
