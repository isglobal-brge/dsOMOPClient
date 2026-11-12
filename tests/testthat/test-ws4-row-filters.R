# WS-4 (design-2.10.3 section 8): defects F-1 (row concept-set filters match
# the table's domain concept column from the role registry) and F-2
# (recipe-level row filters are attached only where their columns exist).

.ws4_v1_plan <- function(recipe) {
  if ("version" %in% names(formals(recipe_to_plan))) {
    recipe_to_plan(recipe, version = 1L)
  } else {
    recipe_to_plan(recipe)
  }
}

test_that("omop_filter_concept_set builds a row concept-set filter", {
  f <- omop_filter_concept_set(c(201820, 201826))
  expect_s3_class(f, "omop_filter")
  expect_identical(f$type, "concept_set")
  expect_identical(f$level, "row")
  expect_identical(f$params$concept_ids, c(201820L, 201826L))
  expect_null(f$params$concept_col)
  g <- omop_filter_concept_set(8554, concept_col = "unit_concept_id")
  expect_identical(g$params$concept_col, "unit_concept_id")
  expect_error(omop_filter_concept_set(integer(0)), "non-negative integer")
  expect_error(omop_filter_concept_set(1.5), "non-negative integer")
  expect_error(omop_filter_concept_set(1, concept_col = "Bad Name"), "column name")
})

test_that("F-1: the concept-set leaf targets the table's domain concept column", {
  f <- omop_filter_concept_set(201820)
  expect_identical(.filter_to_leaf(f, table = "condition_occurrence")$var, "condition_concept_id")
  expect_identical(.filter_to_leaf(f, table = "drug_exposure")$var, "drug_concept_id")
  expect_identical(.filter_to_leaf(f, table = "measurement")$var, "measurement_concept_id")
  # without a known table the sentinel stays; the server resolves it the same way
  expect_identical(.filter_to_leaf(f, table = NULL)$var, "concept_id")
  expect_identical(.filter_to_leaf(omop_filter_concept_set(8554, "unit_concept_id"),
                                   table = "measurement")$var, "unit_concept_id")
  expect_identical(.role_domain_concept("observation_period"), NULL)
  expect_identical(.role_domain_concept("CONDITION_OCCURRENCE"), "condition_concept_id")

  r <- omop_recipe(
    variables = omop_variable(name = "dx", table = "condition_occurrence"),
    filters = omop_filter_concept_set(c(201820, 201826)),
    outputs = omop_output(name = "dx", type = "long"))
  out <- .ws4_v1_plan(r)$outputs$dx
  expect_identical(out$filters$custom$var, "condition_concept_id")
  expect_identical(out$filters$custom$op, "in")
  expect_identical(as.integer(unlist(out$filters$custom$value)), c(201820L, 201826L))
})

test_that("F-2: a recipe-level row filter is never attached to a table lacking its column", {
  ok <- omop_recipe(
    variables = omop_variable(name = "lab", table = "measurement", concept_id = 3004410),
    filters = omop_filter_value_concept(4328749),
    outputs = omop_output(name = "lab", type = "long"))
  expect_identical(.ws4_v1_plan(ok)$outputs$lab$filters$custom$var, "value_as_concept_id")

  bad <- omop_recipe(
    variables = list(
      omop_variable(name = "lab", table = "measurement", concept_id = 3004410),
      omop_variable(name = "dx", table = "condition_occurrence", concept_id = 201820)),
    filters = omop_filter_value_concept(4328749),
    outputs = list(omop_output(name = "lab", type = "long", variables = "lab"),
                   omop_output(name = "dx", type = "long", variables = "dx")))
  expect_error(.ws4_v1_plan(bad),
               "column 'value_as_concept_id' does not exist on table 'condition_occurrence' of output 'dx'")

  # table-agnostic sentinels (date ranges, concept sets) attach everywhere
  dated <- omop_recipe(
    variables = list(
      omop_variable(name = "lab", table = "measurement", concept_id = 3004410),
      omop_variable(name = "dx", table = "condition_occurrence", concept_id = 201820)),
    filters = omop_filter_date_range("2020-01-01", "2020-12-31"),
    outputs = list(omop_output(name = "lab", type = "long", variables = "lab"),
                   omop_output(name = "dx", type = "long", variables = "dx")))
  plan <- .ws4_v1_plan(dated)
  expect_identical(plan$outputs$lab$filters$custom$var, "measurement_date")
  expect_identical(plan$outputs$dx$filters$custom$var, "condition_start_date")

  # columns no CDM table defines (site extensions) are left to the server
  expect_silent(.assert_row_filter_columns(list(var = "site_flag", op = "==", value = 1),
                                           "measurement", "x"))
  expect_error(.assert_row_filter_columns(
    list(not = list(var = "days_supply", op = "==", value = 0)), "measurement", "x"),
    "days_supply")
})

test_that("the CDM column list is vendored with the role registry", {
  cols <- .cdm_table_columns("measurement")
  expect_true(all(c("measurement_id", "value_as_number", "unit_concept_id",
                    "operator_concept_id", "range_low") %in% cols))
  expect_true("episode_event_field_concept_id" %in% .cdm_table_columns("episode_event"))
  expect_identical(.cdm_table_columns("not_a_table"), character(0))
})
