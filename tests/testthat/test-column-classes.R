test_that("client assignment classification matches the shared frozen column corpus", {
  corpus <- utils::read.csv(testthat::test_path("..", "..", "inst", "column_classes.csv"),
                            stringsAsFactors = FALSE)
  for (i in seq_len(nrow(corpus))) {
    column <- corpus$column[[i]]
    blocked <- .is_blocked_column(column)
    quasi <- .is_quasi_identifier_column(column)
    classification <- if (blocked) 1L else if (quasi) 2L else 3L
    expect_identical(classification, corpus$assignment_class[[i]], info = column)
    expect_identical(.is_blocked_column(toupper(column)), blocked, info = column)
    variable <- omop_variable(name = "requested", table = "measurement",
                              column = column, format = "raw")
    recipe <- omop_recipe(variables = variable,
                          outputs = omop_output(name = "rows", type = "long"))
    if (blocked) {
      expect_error(recipe_to_plan(recipe, version = 1), "blocked class-1 column", info = column)
    } else {
      expect_no_error(recipe_to_plan(recipe, version = 1))
    }
    report <- recipe_lint(recipe)
    info <- report[report$code == "quasi_identifier", , drop = FALSE]
    expect_identical(nrow(info), if (quasi) 1L else 0L, info = column)
    if (quasi) {
      expect_identical(info$severity, "INFO", info = column)
      expect_identical(info$locus, "variables$requested", info = column)
      expect_false("no_compile" %in% report$code, info = column)
    }
  }
})

test_that("client class-1 matching preserves reviewed identifiers and birth permission", {
  expect_true(all(vapply(c("birth_date_source_value", "mrn_birth_date",
    "patient_id_raw", "medical_record_number", "email_address", "mobile_phone",
    "result_free_text", "note_nlp_term_modifiers", "member_key", "encounter_id"),
    .is_blocked_column, logical(1))))
  expect_false(any(vapply(c("person_id", "subject_id", "cohort_row_id", "set_id",
    "analysis_id", "cohort_definition_id", "covariate_id", "measurement_id",
    "concept_id", "source_vocabulary_id", "episode_concept_id", "date_of_birth"),
    .is_blocked_column, logical(1))))
  expect_true(.is_blocked_column(NA_character_))
  expect_true(.is_blocked_column(""))
})

test_that("derived age emits informational quasi-identifier lint", {
  recipe <- omop_recipe(variables = omop_variable_age(reference_date = "2020-01-01"),
                        outputs = omop_output(name = "ages", type = "wide"))
  # Lint is advisory and leaves the compiled plan identical.
  before <- recipe_to_plan(recipe, version = 1)
  report <- recipe_lint(recipe)
  expect_identical(report$severity[report$code == "quasi_identifier"], "INFO")
  expect_identical(recipe_to_plan(recipe, version = 1), before)
})
