# A-22 across WS-9 composition: an empty publication list must survive
# merging, binding and the ordinary recipe returned by the simple layer.

.ws9_roots_recipe <- function(names, publish = NULL) {
  omop_recipe(
    variables = list(sex = omop_sex()),
    outputs = lapply(names, function(nm) omop_output(nm, format = "wide")),
    publish = publish)
}

test_that("merging distinguishes all, none and selected roots on either side", {
  left_names <- c("left_one", "left_two")
  right_names <- c("right_one", "right_two")
  choices <- list(NULL, character(0), 1L, 2L)
  for (prefix in list(NULL, "other")) {
    for (left in choices) for (right in choices) {
      a_roots <- if (is.numeric(left)) left_names[left] else left
      b_roots <- if (is.numeric(right)) right_names[right] else right
      a <- .ws9_roots_recipe(left_names, a_roots)
      b <- .ws9_roots_recipe(right_names, b_roots)
      merged <- recipe_merge(a, b, prefix = prefix)
      a_requested <- if (is.null(a_roots)) left_names else a_roots
      b_requested <- if (is.null(b_roots)) right_names else b_roots
      b_outputs <- right_names
      if (!is.null(prefix)) {
        b_outputs <- paste0(prefix, "_", b_outputs)
        # paste0("other_", character(0)) is "other_", not character(0).
        if (length(b_requested)) b_requested <- paste0(prefix, "_", b_requested)
      }
      requested <- c(a_requested, b_requested)
      if (is.null(a_roots) && is.null(b_roots)) {
        expect_null(merged$publish)
      } else {
        expect_identical(merged$publish, requested)
      }
      plan <- recipe_to_plan(merged)
      expect_identical(plan$publish, as.list(requested))
      expect_setequal(names(plan$outputs), c(left_names, b_outputs))
      expect_identical(a$publish, a_roots)
      expect_identical(b$publish, b_roots)
    }
  }
})

test_that("shared outputs are roots only when one merged recipe requests them", {
  all <- .ws9_roots_recipe(c("one", "two"))
  none <- recipe_set_publish(all, character(0))
  selected <- recipe_set_publish(all, "two")
  expect_identical(recipe_merge(none, none)$publish, character(0))
  expect_identical(recipe_to_plan(recipe_merge(none, none))$publish, list())
  expect_identical(recipe_merge(none, all)$publish, c("one", "two"))
  expect_identical(recipe_merge(all, none)$publish, c("one", "two"))
  expect_identical(recipe_merge(none, selected)$publish, "two")
  expect_identical(recipe_merge(selected, none)$publish, "two")
  expect_identical(recipe_merge(selected, selected)$publish, "two")
})

.ws9_roots_study <- function() {
  study <- omop_study(
    index = list(cohort_definition_id = 1L),
    baseline = list(history = omop_ever(
      "condition_occurrence", omop_param("dx", type = "concept_set"),
      before = omop_param("lookback", type = "integer", default = 365L))),
    outputs = "baseline")
  recipe_set_publish(study, character(0))
}

test_that("binding a private simple-layer recipe keeps roots and validates unused definitions", {
  study <- .ws9_roots_study()
  expect_identical(study$publish, character(0))
  expect_error(recipe_to_plan(study), "Unbound parameter 'dx'")
  partial <- recipe_bind(study, lookback = 30L)
  expect_identical(partial$publish, character(0))
  expect_identical(names(recipe_params(partial)), "dx")
  bound <- recipe_bind(partial, dx = 201826L)
  plan <- recipe_to_plan(bound)
  expect_identical(plan$publish, list())
  expect_identical(plan$variables$history$reducer$window$start, -30L)
  expect_identical(plan, recipe_to_plan(recipe_bind(study, lookback = 30L, dx = 201826L)))
  expect_identical(recipe_to_plan(recipe_bind(study, dx = 201826L))$variables$history$reducer$window$start,
                   -365L)
  expect_error(recipe_bind(study, lookback = -1L), "start must not be after end")
  expect_error(recipe_bind(study, dx = TRUE), "concept")
  expect_identical(recipe_bind(study)$publish, character(0))
  expect_null(recipe_bind(recipe_set_publish(study, NULL), dx = 201826L)$publish)
  expect_identical(study$publish, character(0))
})

test_that("merged private studies retain roots and shared parameters in JSON, YAML and R code", {
  study <- .ws9_roots_study()
  merged <- recipe_merge(study, study, prefix = "second")
  expect_identical(merged$publish, character(0))
  expect_setequal(names(recipe_params(merged)), c("dx", "lookback"))
  target <- recipe_to_plan(recipe_bind(merged, dx = 201826L, lookback = 30L))
  expect_identical(target$publish, list())
  expect_identical(names(target$outputs), c("baseline", "second_baseline"))
  expect_identical(target$outputs$baseline$columns, list("history"))
  expect_identical(target$outputs$second_baseline$columns, list("second_history"))

  check <- function(restored) {
    expect_identical(restored$publish, character(0))
    expect_setequal(names(recipe_params(restored)), c("dx", "lookback"))
    actual <- recipe_to_plan(recipe_bind(restored, dx = 201826L, lookback = 30L))
    expect_identical(actual, target)
    expect_identical(dsOMOPClient:::.plan_v2_digest(actual)$sha256,
                     dsOMOPClient:::.plan_v2_digest(target)$sha256)
  }
  check(recipe_import_json(recipe_export_json(merged)))
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(merged)), envir = env)
  check(env$recipe)
  # WS-8's single named-variable code generation fix must also hold for a
  # parameterised simple-layer recipe before any merge makes it multi-variable.
  eval(parse(text = recipe_to_code(study)), envir = env)
  expect_identical(env$recipe$publish, character(0))
  expect_identical(recipe_to_plan(recipe_bind(env$recipe, dx = 201826L)),
                   recipe_to_plan(recipe_bind(study, dx = 201826L)))
  skip_if_not_installed("yaml")
  check(recipe_import_yaml(recipe_export_yaml(merged)))
})
