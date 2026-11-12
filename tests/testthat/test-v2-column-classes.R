# WS-8 (design-2.10.3 section 7.6, F-3): class 1 is refused at compile time in
# v1 and language-2 plans, class 2 is permitted and reported. The column
# classes are R/assignment_policy.R's, checked against the shared corpus
# (inst/column_classes.csv) row by row in test-column-classes.R.

.column_corpus <- function() {
  cc <- system.file("column_classes.csv", package = "dsOMOPClient")
  if (!nzchar(cc)) cc <- testthat::test_path("..", "..", "inst", "column_classes.csv")
  utils::read.csv(cc, stringsAsFactors = FALSE)
}

test_that("class-2 columns are quasi-identifiers, never blocked on assignment paths", {
  rows <- .column_corpus()
  for (col in rows$column[rows$assignment_class == 2L & rows$context != "cdm" |
                            rows$column %in% c("year_of_birth", "month_of_birth", "day_of_birth",
                                               "birth_datetime", "age", "age_group",
                                               "birth_year_band")]) {
    expect_false(dsOMOPClient:::.is_blocked_column(col), info = col)
  }
  for (col in c("year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime", "dob",
                "date_of_birth", "birth_date", "age", "age_group", "birth_year_band")) {
    expect_true(dsOMOPClient:::.is_quasi_identifier_column(col), info = col)
  }
  expect_false(dsOMOPClient:::.is_quasi_identifier_column("gender_concept_id"))
  # a birth-named source value stays class 1
  expect_true(dsOMOPClient:::.is_blocked_column("birth_date_source_value"))
  expect_false(dsOMOPClient:::.is_quasi_identifier_column("birth_date_source_value"))
})

test_that("compile-time refusal covers the newly listed class-1 columns (F-3)", {
  for (col in c("stop_reason", "lot_number", "unique_device_id", "note_title",
                "condition_source_concept_id", "city", "npi", "care_site_name", "mrn")) {
    r <- omop_recipe(variables = omop_variable(name = "x", table = "drug_exposure", column = col),
                     outputs = omop_output("w", type = "long"))
    expect_error(recipe_to_plan(r, version = 1), "blocked", info = col)
  }
  ok <- omop_recipe(variables = omop_variable(name = "yob", table = "person",
                                              column = "year_of_birth"),
                    outputs = omop_output("w", type = "wide"))
  expect_s3_class(recipe_to_plan(ok, version = 1), "omop_plan")
})

test_that("language-2 plans refuse class-1 projections, value columns and predicates", {
  raw <- omop_recipe(variables = omop_variable(name = "src", table = "person",
                                               column = "person_source_value"),
                     outputs = omop_output("w", format = "wide"))
  expect_error(recipe_to_plan(raw), "blocked \\(class-1\\).*person_source_value")
  val <- omop_recipe(variables = list(x = omop_variable(
    event = omop_event("observation", 1, value = "value_as_string"), reducer = omop_reduce("count"))),
    outputs = omop_output("w", format = "wide"))
  expect_error(recipe_to_plan(val), "value_as_string")
  txt <- omop_recipe(variables = list(x = omop_variable(
    event = omop_event("drug_exposure", 1, where = omop_where(sig == "bid")),
    reducer = omop_reduce("count"))), outputs = omop_output("w", format = "wide"))
  expect_error(recipe_to_plan(txt), "sig")
  ok <- omop_recipe(populations = omop_population("p", filters = list(
    omop_person(omop_where(year_of_birth >= 1950)))),
    variables = list(yob = omop_variable(name = "yob", table = "person", column = "year_of_birth")),
    outputs = omop_output("w", rows = "p", format = "wide"))
  plan <- recipe_to_plan(ok)
  expect_identical(plan$variables$yob, list(raw = list(table = "person", column = "year_of_birth")))
})
