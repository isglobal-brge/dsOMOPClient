# ==============================================================================
# Tests for recipe_preview_schema() enriched output
# ==============================================================================

# Build the B1 recipe used across schema tests.
.b1_recipe <- function() {
  recipe <- omop_recipe()
  recipe <- dsOMOPClient:::recipe_add_filter(recipe,
    omop_filter_has_concept(320128, "condition_occurrence"))
  recipe <- dsOMOPClient:::recipe_add_variable(recipe, omop_variable_sex(name = "sex"))
  recipe <- dsOMOPClient:::recipe_add_variable(recipe,
    omop_variable_age(name = "age", reference = "index"))
  recipe <- dsOMOPClient:::recipe_add_variable(recipe,
    omop_variable(name = "n_rhythm", table = "measurement",
                  concept_id = 3022318, concept_name = "Heart rate rhythm",
                  format = "count"))
  dsOMOPClient:::recipe_add_output(recipe, omop_output(name = "wide", type = "wide"))
}

test_that("recipe_preview_schema keeps schema columns and output attrs", {
  s <- recipe_preview_schema(.b1_recipe())[["wide"]]
  expect_true(all(c("output", "column", "source", "concept", "type",
                    "format") %in% names(s)))
  expect_equal(attr(s, "join_key"), "cohort_row_id")
  expect_equal(attr(s, "output_type"), "wide")
  expect_equal(attr(s, "population_id"), "base")
  expect_true(all(c("person", "measurement") %in% attr(s, "tables")))
})

test_that("recipe_preview_schema reports the released pseudonymous person key", {
  s <- recipe_preview_schema(.b1_recipe())[["wide"]]
  person <- s[s$column == "person_id", , drop = FALSE]
  expect_equal(person$source, "person.person_id")
  expect_equal(person$r_type, "character")
})

test_that("recipe_preview_schema maps B1 formats to R types with concept names", {
  s <- recipe_preview_schema(.b1_recipe())[["wide"]]
  row <- function(col) s[s$column == col, , drop = FALSE]

  expect_equal(row("sex")$r_type, "factor")
  expect_equal(row("age")$r_type, "numeric")

  nr <- row("n_rhythm")
  expect_equal(nr$r_type, "integer")
  expect_equal(nr$concept_name, "Heart rate rhythm")
  expect_equal(nr$concept, "3022318")
  expect_equal(nr$format, "count")
})

test_that("recipe preview accepts feature windows only at episode grain", {
  recipe <- omop_recipe()
  recipe <- dsOMOPClient:::recipe_add_variable(recipe,
    omop_variable(name = "sbp", table = "measurement", concept_id = 3004249,
                  concept_name = "Systolic BP", format = "mean",
                  time_window = list(start = -365, end = 0)))
  recipe <- dsOMOPClient:::recipe_add_output(recipe, omop_output(name = "w", type = "wide"))
  expect_error(recipe_preview_schema(recipe),
               "grain='episode'")

  recipe$outputs$w$options <- list(grain = "episode")
  schema <- recipe_preview_schema(recipe)$w
  expect_true(all(c("cohort_row_id", "person_id", "sbp") %in% schema$column))
  expect_equal(attr(schema, "join_key"), "cohort_row_id")
  expect_equal(schema[schema$column == "cohort_row_id", "r_type"], "integer")
  expect_equal(schema[schema$column == "person_id", "r_type"], "character")
})

test_that("recipe_preview_schema preserves episode keys for longitudinal rows", {
  recipe <- omop_recipe(
    variables = omop_variable(
      name = "diagnosis", table = "condition_occurrence",
      concept_id = 201820, column = "condition_concept_id", format = "raw",
      time_window = list(start = -30L, end = 0L)
    ),
    outputs = omop_output(name = "events", type = "long")
  )

  schema <- recipe_preview_schema(recipe)$events
  expect_true(all(c("cohort_row_id", "person_id",
                    "condition_concept_id") %in% schema$column))
  expect_equal(attr(schema, "join_key"), "cohort_row_id")
})

test_that("recipe_preview_schema rejects one output with mixed join grains", {
  recipe <- omop_recipe(
    variables = list(
      omop_variable(
        name = "all_history", table = "condition_occurrence",
        concept_id = 201820, column = "condition_concept_id", format = "raw"
      ),
      omop_variable(
        name = "recent", table = "condition_occurrence",
        concept_id = 255573, column = "condition_concept_id", format = "raw",
        time_window = list(start = -30L, end = 0L)
      )
    ),
    outputs = omop_output(name = "events", type = "long")
  )

  expect_error(recipe_preview_schema(recipe),
               "incompatible person and episode grains")
})

test_that("recipe_preview_schema exposes baseline's released age-group schema", {
  recipe <- omop_recipe(
    variables = omop_variable_age(name = "age", reference = "index"),
    outputs = omop_output(name = "baseline", type = "baseline")
  )

  schema <- recipe_preview_schema(recipe)$baseline
  expect_true(all(c("row_id", "cohort_row_id", "person_id", "age_group") %in%
                    schema$column))
  expect_false(any(c("age", "age_at_index") %in% schema$column))
  expect_equal(attr(schema, "join_key"), "cohort_row_id")
  expect_equal(schema[schema$column == "person_id", "r_type"], "character")
  expect_equal(schema[schema$column == "age_group", "r_type"], "character")
})

test_that("recipe_preview_schema reports fixed longitudinal output types", {
  outcome <- omop_variable(
    name = "outcome", table = "condition_occurrence",
    concept_id = 201820, format = "binary"
  )
  survival <- recipe_preview_schema(omop_recipe(
    variables = outcome,
    outputs = omop_output(name = "tte", type = "survival")
  ))$tte

  expect_equal(
    survival$column,
    c("row_id", "cohort_row_id", "person_id", "event",
      "time_to_event_days")
  )
  expect_equal(attr(survival, "join_key"), "cohort_row_id")
  expect_equal(survival[survival$column == "person_id", "r_type"], "character")
  expect_equal(survival[survival$column == "time_to_event_days", "r_type"],
               "integer")

  interval <- omop_variable(
    name = "exposure", table = "drug_exposure",
    concept_id = 1124300, format = "raw"
  )
  intervals <- recipe_preview_schema(omop_recipe(
    variables = interval,
    outputs = omop_output(name = "intervals", type = "intervals")
  ))$intervals

  expect_true(all(c("row_id", "cohort_row_id", "subject_id",
                    "start_days_from_index", "end_days_from_index") %in%
                    intervals$column))
  expect_equal(attr(intervals, "join_key"), "cohort_row_id")
  expect_equal(intervals[intervals$column == "subject_id", "r_type"],
               "character")
  expect_true(all(intervals[
    intervals$column %in% c("start_days_from_index", "end_days_from_index"),
    "r_type"
  ] == "integer"))
})

test_that("recipe_preview_schema types temporal linkage after release", {
  recipe <- omop_recipe(
    variables = omop_variable(
      name = "diagnosis", table = "condition_occurrence",
      concept_id = 201820, format = "binary"
    ),
    outputs = omop_output(name = "temporal", type = "temporal_covariates")
  )

  schema <- recipe_preview_schema(recipe)$temporal
  rtype <- function(column) schema[schema$column == column, "r_type"]
  expect_equal(attr(schema, "join_key"), "rowId")
  expect_equal(rtype("personRef.person_id"), "character")
  expect_equal(rtype("personRef.rowId"), "integer")
  expect_equal(rtype("timeRef.startDay"), "integer")
  expect_equal(rtype("covariateRef.analysisId"), "integer")
  expect_equal(rtype("temporalCovariates.covariateValue"), "numeric")
})

test_that("recipe_preview_schema does not invent unmaterialized suffix columns", {
  recipe <- omop_recipe()
  mv <- omop_variable(name = "hba1c", table = "measurement",
                      concept_id = 3004410, concept_name = "HbA1c",
                      format = "mean")
  mv[[".suffix_names"]] <- c("hba1c_1", "hba1c_2", "hba1c_3")
  recipe <- dsOMOPClient:::recipe_add_variable(recipe, mv)
  recipe <- dsOMOPClient:::recipe_add_output(recipe, omop_output(name = "w", type = "wide"))
  s <- recipe_preview_schema(recipe)[["w"]]
  expect_true("hba1c" %in% s$column)
  expect_false(any(c("hba1c_1", "hba1c_2", "hba1c_3") %in% s$column))
  expect_equal(nrow(s[s$concept_name == "HbA1c", ]), 1)
})

test_that("recipe_preview_schema marks table_split for multi-table long outputs", {
  recipe <- omop_recipe()
  recipe <- dsOMOPClient:::recipe_add_variable(recipe,
    omop_variable(name = "hr", table = "measurement", concept_id = 3022318,
                  concept_name = "Heart rate", column = "value_as_number",
                  format = "raw"))
  recipe <- dsOMOPClient:::recipe_add_variable(recipe,
    omop_variable(name = "dx", table = "condition_occurrence",
                  concept_id = 201820, concept_name = "T2DM",
                  column = "condition_concept_id", format = "raw"))
  recipe <- dsOMOPClient:::recipe_add_output(recipe, omop_output(name = "evt", type = "long"))
  s <- recipe_preview_schema(recipe)[["evt"]]
  expect_true("table_split" %in% names(s))
  expect_setequal(unique(s$output),
                  c("evt_measurement", "evt_condition_occurrence"))
  expect_equal(s[s$column == "value_as_number", "table_split"], "measurement")
  expect_equal(s[s$column == "condition_concept_id", "table_split"],
               "condition_occurrence")
  expect_false(any(c("hr", "dx") %in% s$column, na.rm = TRUE))
})

test_that("recipe_preview_schema handles empty recipe and variable-less output", {
  expect_equal(recipe_preview_schema(omop_recipe()), list())

  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "empty", type = "wide"))
  expect_error(recipe_preview_schema(r), "has no variables to compile")
})

test_that("recipe_preview_schema rejects non-recipe input", {
  expect_error(recipe_preview_schema(list()), "omop_recipe")
})
