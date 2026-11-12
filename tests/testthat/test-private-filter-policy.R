test_that("ordinary private numeric predicates compile and lint without safe bins", {
  for (bounds in list(list(min_value = 6.5), list(max_value = 10),
                      list(min_value = 6.5, max_value = 10))) {
    criterion <- do.call(omop_filter_has_measurement,
                         c(list(concept_id = 3004410L), bounds))
    recipe <- omop_recipe(
      variables = omop_variable(name = "value", table = "measurement",
                                column = "value_as_number"),
      filters = list(criterion, omop_filter_age(40, 41),
                     omop_filter_value(threshold = 6.5)),
      outputs = omop_output(name = "rows", type = "long"))
    plan <- recipe_to_plan(recipe, version = 1)
    expect_identical(plan$outputs$rows$filters$custom,
                     list(var = "value_as_number", op = ">=", value = 6.5))
    expect_false(any(recipe_lint(recipe)$code %in%
                       c("narrow_filter", "unbound_numeric_filter", "no_compile")))
    expect_null(criterion$params$safe_scope)
    for (name in names(bounds)) expect_identical(criterion$params[[name]], bounds[[name]])
  }
  expect_error(omop_filter_has_measurement(3004410L, min_value = Inf), "finite numeric")
  expect_error(omop_filter_has_measurement(3004410L, min_value = c(1, 2)), "finite numeric")
  expect_error(omop_filter_has_measurement(3004410L, min_value = 6, max_value = 4),
               "min_value < max_value")
})

test_that("Circe numeric conversion preserves endpoint and authentication semantics", {
  make_recipe <- function(filter) omop_recipe(
    populations = omop_population(id = "p",
      index_event = omop_index_event(201820, "condition_occurrence"),
      filters = filter),
    outputs = omop_output(type = "wide", population_id = "p"))
  expect_error(recipe_export_circe(make_recipe(
    omop_filter_has_measurement(3004410L, min_value = 4, max_value = 6)), "p"),
    "half-open numeric range")
  expect_error(recipe_export_circe(make_recipe(
    omop_filter_has_measurement(3004410L, min_value = 4, max_value = 6,
      safe_bins = .test_safe_bins(c(0, 4, 6, 10)))), "p"),
    "authenticated safe-bin contract")

  filter <- omop_filter_has_measurement(3004410L, min_value = 4)
  converted <- .circe_measurement_from_filter(filter, list())
  expect_identical(converted$criteria$Criteria$Measurement$ValueAsNumber,
                   list(Value = 4, Op = "gte"))
  for (operator in c("gt", "lte", "eq", "bt")) {
    changed <- converted$criteria
    changed$Criteria$Measurement$ValueAsNumber$Op <- operator
    expect_error(.circe_criteria_to_filter(changed, converted$sets),
                 "endpoint semantics", info = operator)
  }
  changed <- converted$criteria
  changed$Occurrence <- list(Type = 0L, Count = 0L)
  expect_error(.circe_criteria_to_filter(changed, converted$sets),
               "numeric measurement criteria require occurrence")
})
