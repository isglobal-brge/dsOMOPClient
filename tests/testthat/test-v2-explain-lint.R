# WS-8 (design-2.10.3 sections 6.15, 7.6, 10.1; F-3, F-4, F-5, B-7, B-8, F-23):
# explain and lint surface defaults, report column classes, mirror no gates.

.lint_codes <- function(r) recipe_lint(r)$code

test_that("lint states the legacy 730-day TAR, baseline age bands and coverage defaults", {
  surv <- omop_recipe(
    variables = omop_variable(name = "mi", table = "condition_occurrence", concept_id = 4329847),
    outputs = omop_output("s", type = "survival"))
  L <- recipe_lint(surv)
  expect_true("tar_default" %in% L$code)
  expect_match(L$message[L$code == "tar_default"], "730 days after index")
  expect_identical(L$severity[L$code == "tar_default"], "INFO")
  expect_true("censoring_default" %in% L$code)
  base <- omop_recipe(variables = omop_variable_age(reference = "index"),
                      outputs = omop_output("b", type = "baseline"))
  expect_true("age_group_default" %in% .lint_codes(base))
  ev <- omop_event("condition_occurrence", 1, name = "dx")
  idx <- omop_recipe(populations = omop_population("p", index = omop_index(ev)),
                     variables = list(sex = omop_sex()),
                     outputs = list(omop_output("w", rows = "p", format = "wide"),
                                    omop_output("pp", rows = "p", format = "wide", grain = "person",
                                                collapse = "last_episode")))
  codes <- .lint_codes(idx)
  expect_true(all(c("coverage", "collapse") %in% codes))
  expect_false(any(recipe_lint(idx)$severity == "ERROR"))
})

test_that("class-2 columns are INFO quasi_identifier; class-1 requests fail to compile", {
  r <- omop_recipe(variables = list(
    omop_variable(name = "yob", table = "person", column = "year_of_birth"),
    age = omop_age()), outputs = omop_output("w", format = "wide"))
  L <- recipe_lint(r)
  expect_true(sum(L$code == "quasi_identifier") >= 2L)
  expect_false("no_compile" %in% L$code)
  bad <- omop_recipe(variables = omop_variable(name = "src", table = "person",
                                               column = "person_source_value"),
                     outputs = omop_output("w"))
  expect_true("no_compile" %in% .lint_codes(bad))
})

test_that("legacy measurement pair and parent inheritance are explained (B-7, B-8)", {
  r <- omop_recipe(
    populations = list(
      omop_population("a", filters = list(omop_filter_missing_measurement(3004410))),
      omop_population("b", parent_id = "a", filters = list(omop_filter_sex("F")))),
    variables = omop_variable_sex(),
    outputs = omop_output("w", type = "wide", population_id = "b"))
  codes <- .lint_codes(r)
  expect_true(all(c("measurement_pair", "parent_inheritance") %in% codes))
  expect_false(any(c("narrow_filter", "unbound_numeric_filter") %in% codes))
})

test_that("recipe_explain renders the normalised plan with its defaults, never counts", {
  mi <- omop_event("condition_occurrence", 4329847, name = "mi")
  r <- omop_recipe(
    populations = omop_population("c1", index = omop_index(list(cohort_definition_id = 1),
                                                           "first_eligible",
                                                           end = omop_end("event_end")),
                                  filters = list(omop_observed(before = 365))),
    variables = list(age = omop_age(), last = omop_variable(
      event = omop_event("measurement", 3004410, name = "a1c"),
      reducer = omop_reduce("last_value", window = omop_window(-365, 0)))),
    outputs = list(omop_output("t1", rows = "c1", format = "wide"),
                   omop_output("tte", rows = "c1", format = "survival",
                               options = list(outcomes = list(mi = mi), censor = "observation_end"))))
  ex <- recipe_explain(r, detail = "semantics")
  expect_s3_class(ex, "omop_recipe_explain")
  txt <- paste(ex$text, collapse = "\n")
  expect_match(txt, "select = \"first_eligible\"")
  expect_match(txt, "coverage: unique/drop/drop")
  expect_match(txt, "730 days after index")
  expect_match(txt, "only these cap follow-up")
  expect_match(txt, "exact annual age")
  expect_match(txt, "missing = \"skip\"")
  expect_match(txt, "Published: t1, tte")
  expect_false(grepl("[0-9]+ (persons|rows|records)", txt))
  expect_true(all(c("tar_default", "coverage", "missing_skip") %in% ex$defaults$code))
  expect_identical(ex$digest, dsOMOPClient:::.plan_v2_digest(recipe_to_plan(r))$sha256)
  expect_output(print(ex), "Population 'c1'")
  summary_ex <- recipe_explain(r)
  expect_lt(length(summary_ex$text), length(ex$text))
})

test_that("a 2.10.1 recipe without a v2 lowering is explained from its v1 shape", {
  r <- omop_recipe(variables = omop_variable(table = "measurement", concept_id = 1),
                   outputs = omop_output("l", type = "long"))
  ex <- recipe_explain(r)
  expect_match(ex$text[[1]], "unchanged v1 plan")
})
