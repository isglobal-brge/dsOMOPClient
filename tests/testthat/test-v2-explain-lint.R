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

test_that("lint compiles as execution does: newly permitted 2.10.1 shapes are language_2_only", {
  r <- omop_recipe(variables = omop_variable_age(),
                   filters = omop_filter_group(omop_filter_sex("F"), operator = "NOT"),
                   outputs = omop_output(name = "age", type = "wide"))
  L <- recipe_lint(r)
  expect_false("no_compile" %in% L$code)
  expect_identical(L$severity[L$code == "language_2_only"], "INFO")
  expect_match(L$message[L$code == "language_2_only"], "2.10.4 servers only")
  plain <- omop_recipe(variables = omop_variable_age(), outputs = omop_output(name = "age", type = "wide"))
  expect_false("language_2_only" %in% recipe_lint(plain)$code)
})

test_that("explain selects the plan exactly as execution does (A-14)", {
  # a recipe of 2.10.1 constructors that also has a native rendering is
  # explained as the v1 plan it executes as, never as that rendering
  legacy <- omop_recipe(variables = omop_variable_sex(), outputs = omop_output("w", type = "wide"))
  expect_s3_class(recipe_to_plan(legacy), "omop_plan_v2")
  ex <- recipe_explain(legacy)
  expect_identical(ex$selected, "v1")
  expect_s3_class(ex$payload, "omop_plan")
  expect_null(ex$digest)
  expect_null(ex$plan)
  expect_match(paste(ex$text, collapse = "\n"), "not available offline")
  # a newly permitted shape and a language-2 recipe are explained as v2
  negated <- omop_recipe(variables = omop_variable_age(),
                         filters = omop_filter_group(omop_filter_sex("F"), operator = "NOT"),
                         outputs = omop_output(name = "age", type = "wide"))
  ex2 <- recipe_explain(negated)
  expect_identical(ex2$selected, "v2")
  expect_identical(ex2$digest, dsOMOPClient:::.plan_v2_digest(recipe_to_plan(negated))$sha256)
})

.explain_inventory <- function(explain_sites) {
  function(conns, ...) {
    inv <- .current_published_methods(conns)
    for (s in setdiff(names(inv), explain_sites)) {
      inv[[s]]$aggregate <- setdiff(inv[[s]]$aggregate, "omopPlanExplainDS")
    }
    inv
  }
}

test_that("server explain receives the selected wire plan; missing explain is reported (B7)", {
  .with_caps(list(a = .caps(), b = .caps()))
  sent <- NULL
  testthat::local_mocked_bindings(
    .dsomop_published_methods = .explain_inventory("a"),
    .ds_safe_aggregate = function(conns, expr, ...) {
      sent <<- expr
      stats::setNames(lapply(names(conns), function(s) {
        list(semantic_digest = "d1", plan = list(populations = list(base = list(
          kind = "index", index = list(source = list(legacy_spec = list()))))))
      }), names(conns))
    }, .package = "dsOMOPClient")
  legacy <- omop_recipe(variables = omop_variable_sex(), outputs = omop_output("w", type = "wide"))
  ex <- recipe_explain(legacy, conns = list(a = "A", b = "B"))
  expect_identical(ex$selected, "v1")
  expect_s3_class(ex$payload, "omop_plan")             # the v1 payload is kept
  expect_identical(as.character(sent[[1L]]), "omopPlanExplainDS")
  expect_null(.decode(sent[[3L]])$version)             # the v1 plan, as executed
  expect_identical(ex$digest, "d1")
  txt <- paste(ex$text, collapse = "\n")
  expect_match(txt, "compatibility variants: legacy_spec")
  expect_match(txt, "Server explain is unavailable on b \\(the profile does not publish omopPlanExplainDS\\)")
  expect_named(ex$server, "a")
})

test_that("explain reports partial transport failures and missing methods by site (B7)", {
  .with_caps(list(a = .caps(), b = .caps()))
  testthat::local_mocked_bindings(
    .dsomop_published_methods = .current_published_methods,
    .ds_safe_aggregate = function(conns, expr, ...) {
      structure(list(a = list(semantic_digest = "d1", plan = list())),
                ds_errors = list(b = "Error while evaluating -> Error : transport lost"))
    }, .package = "dsOMOPClient")
  legacy <- omop_recipe(variables = omop_variable_sex(), outputs = omop_output("w", type = "wide"))
  ex <- recipe_explain(legacy, conns = list(a = "A", b = "B"))
  txt <- paste(ex$text, collapse = "\n")
  expect_match(txt, "Server explain failed on b: transport lost")
  expect_identical(ex$digest, "d1")                    # the site that answered
  # no site explains: the v1 payload stays, IR and digest are unavailable
  testthat::local_mocked_bindings(.dsomop_published_methods = .explain_inventory(character(0)),
                                  .package = "dsOMOPClient")
  ex2 <- recipe_explain(legacy, conns = list(a = "A", b = "B"))
  expect_identical(ex2$selected, "v1")
  expect_s3_class(ex2$payload, "omop_plan")
  expect_null(ex2$digest)
  expect_null(ex2$plan)
  txt2 <- paste(ex2$text, collapse = "\n")
  expect_match(txt2, "Server explain is unavailable on a, b")
  expect_match(txt2, "lowered IR and semantic digest of this v1 plan are unavailable: no site explained it")
})

test_that("all failed server explanations preserve v1 payloads and the local v2 digest (B7)", {
  .with_caps(list(a = .caps(), b = .caps()))
  sent <- list()
  testthat::local_mocked_bindings(
    .dsomop_published_methods = .current_published_methods,
    .ds_safe_aggregate = function(conns, expr, ...) {
      sent[[length(sent) + 1L]] <<- expr
      structure(list(), ds_errors = list(a = "Error : disconnected", b = "Error : timed out"))
    }, .package = "dsOMOPClient")
  v1 <- omop_recipe(variables = omop_variable_sex(), outputs = omop_output("w", type = "wide"))
  ex1 <- recipe_explain(v1, conns = list(a = "A", b = "B"))
  expect_identical(ex1$payload, recipe_to_plan(v1, version = 1L))
  expect_identical(sent[[1L]][[3L]], dsOMOPClient:::.ds_encode(ex1$payload))
  expect_null(ex1$plan)
  expect_null(ex1$digest)
  expect_match(paste(ex1$text, collapse = "\n"), "Server explain failed on a: disconnected")
  expect_match(paste(ex1$text, collapse = "\n"), "Server explain failed on b: timed out")
  expect_match(paste(ex1$text, collapse = "\n"), "unavailable: no site explained it")

  v2 <- omop_recipe(variables = list(sex = omop_sex()), outputs = omop_output("w", format = "wide"))
  ex2 <- recipe_explain(v2, conns = list(a = "A", b = "B"))
  expect_identical(ex2$selected, "v2")
  expect_identical(ex2$digest, dsOMOPClient:::.plan_v2_digest(recipe_to_plan(v2))$sha256)
  expect_identical(sent[[2L]][[3L]], dsOMOPClient:::.ds_encode_lossless(ex2$payload))
  expect_match(paste(ex2$text, collapse = "\n"), "Server explain failed on b: timed out")
})

test_that("v1 explanation reports disagreement and method-listing failures (B7)", {
  .with_caps(list(a = .caps(), b = .caps()))
  testthat::local_mocked_bindings(
    .dsomop_published_methods = .current_published_methods,
    .ds_safe_aggregate = function(conns, expr, ...) list(
      a = list(semantic_digest = "d1", plan = list()),
      b = list(semantic_digest = "d2", plan = list())),
    .package = "dsOMOPClient")
  r <- omop_recipe(variables = omop_variable_sex(), outputs = omop_output("w"))
  ex <- recipe_explain(r, conns = list(a = "A", b = "B"))
  expect_null(ex$plan)
  expect_null(ex$digest)
  expect_match(paste(ex$text, collapse = "\n"), "the sites that explained it disagree")

  testthat::local_mocked_bindings(
    .dsomop_published_methods = function(conns, ...) {
      inv <- .current_published_methods(conns)
      inv$b$error <- "Error : profile inventory timed out"
      inv
    }, .package = "dsOMOPClient")
  ex2 <- recipe_explain(r, conns = list(a = "A", b = "B"))
  expect_match(paste(ex2$text, collapse = "\n"),
               "Server explain is unavailable on b \\(the profile's methods could not be listed: profile inventory timed out\\)")
  expect_identical(ex2$payload, recipe_to_plan(r, version = 1L))
})

test_that("explain checks the factor methods required by ordinary memory execution", {
  for (method in c("omopFactorLevelsDS", "omopAsFactorColumnsDS")) {
    state <- .legacy_federation(new = "a", unpublished = method)
    testthat::local_mocked_bindings(
      .ds_safe_aggregate = function(...) list(a = list()), .package = "dsOMOPClient")
    recipes <- list(
      omop_recipe(variables = omop_variable_sex(), outputs = omop_output("w")),
      omop_recipe(variables = list(sex = omop_sex()), outputs = omop_output("w", format = "wide")))
    for (r in recipes) {
      ex <- recipe_explain(r, conns = list(a = "a"))
      expect_match(ex$refusal, paste0("a lacks ", method))
      expect_match(paste(ex$text, collapse = "\n"), "Execution on these servers would be refused")
      expect_error(recipe_execute(r), paste0("a lacks ", method))
      expect_identical(unname(state$assigns), 0L)

      enabled <- recipe_explain(recipe_set_options(r, factor_concepts = FALSE), conns = list(a = "a"))
      expect_null(enabled$refusal)
      expect_match(paste(enabled$text, collapse = "\n"),
                   "Execution would use the publication protocol on every site")
    }
    empty <- dsOMOPClient:::recipe_set_publish(recipes[[2L]], character(0))
    expect_null(recipe_explain(empty, conns = list(a = "a"))$refusal)
  }
})
