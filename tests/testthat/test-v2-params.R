# WS-9 parameters (design-2.10.3 section 2.10, amendment A-13): omop_param()
# placeholders, recipe_params(), recipe_bind() and their serialisation.

.prm_recipe <- function(dx = omop_param("dx", type = "concept_set"),
                        lookback = omop_param("lookback", type = "integer", default = 365L)) {
  omop_recipe(
    populations = list(omop_population("c1", index = omop_index(
      list(cohort_definition_id = omop_param("cohort", type = "integer", default = 1)),
      select = "all", end = omop_end("event_end")))),
    variables = list(dm = omop_variable(
      event = omop_event("condition_occurrence", dx, name = "dm"),
      reducer = omop_reduce("binary", window = omop_window(-lookback, 0)))),
    outputs = list(omop_output("w", rows = "c1", format = "wide", grain = "episode")))
}

.prm_bound_plan <- function(dx = omop_concept_set(201820, descendants = TRUE), lookback = 365L,
                            cohort = 1L) {
  recipe_to_plan(omop_recipe(
    populations = list(omop_population("c1", index = omop_index(
      list(cohort_definition_id = cohort), select = "all", end = omop_end("event_end")))),
    variables = list(dm = omop_variable(
      event = omop_event("condition_occurrence", dx, name = "dm"),
      reducer = omop_reduce("binary", window = omop_window(-lookback, 0)))),
    outputs = list(omop_output("w", rows = "c1", format = "wide", grain = "episode"))))
}

test_that("omop_param() declares a typed placeholder; defaults are type-checked", {
  p <- omop_param("lookback", type = "integer", default = 365, description = "days")
  expect_s3_class(p, "omop_param")
  expect_identical(p$default, 365L)
  expect_identical(omop_param("x")$type, "concept_set")   # design 2.10: the first type
  expect_error(omop_param("1x"), "must be one name")
  expect_error(omop_param("x", type = "bogus"), "type must be one of")
  expect_error(omop_param("x", type = "integer", default = 1.5), "exact integer")
  expect_error(omop_param("x", type = "window", default = 3), "omop_window")
  expect_error(omop_param("w", type = "window",
                          default = omop_window(-omop_param("n", type = "integer"), 0)),
               "cannot hold a parameter \\('n'\\)")
  expect_error(omop_param("x", type = "date", default = "2020-02-30"), "valid calendar date")
  expect_identical(omop_param("d", type = "date", default = as.Date("2020-01-31"))$default,
                   "2020-01-31")
  expect_identical(omop_param("c", type = "concept_set", default = c(3, 1, 3))$default, c(3L, 1L))
  expect_output(print(p), "omop_param 'lookback' \\(integer, default 365\\): days")
})

test_that("numeric placeholders admit + - * / and unary minus; nothing else applies", {
  lb <- omop_param("lookback", type = "integer", default = 365L)
  neg <- -lb
  expect_s3_class(neg, "omop_param_op")
  expect_identical(neg$type, "integer")
  expect_identical((lb * -1L)$type, "integer")
  expect_identical((lb + 1e10)$type, "integer")
  expect_identical((lb / 2L)$type, "number")
  expect_identical((omop_param("n", type = "number") + 1L)$type, "number")
  expect_identical(+lb, lb)
  expect_error(lb > 1, "only \\+ - \\* /")
  expect_error(lb & TRUE, "only \\+ - \\* /")
  expect_error(omop_param("cs", type = "concept_set") * 2, "integer and number parameters only")
  expect_error(lb + "a", "finite numeric literals")
  expect_output(print(lb * -1L), "<param lookback> \\* -1")
  w <- omop_window(lb * -1L, 0)
  expect_s3_class(w$start, "omop_param_op")
  expect_output(print(w), "<param lookback> \\* -1")
})

test_that("recipe_params() lists every placeholder once and refuses two definitions of one name", {
  rec <- .prm_recipe()
  params <- recipe_params(rec)
  expect_setequal(names(params), c("dx", "lookback", "cohort"))
  expect_identical(params$lookback$default, 365L)
  bad <- .prm_recipe(lookback = omop_param("dx", type = "integer"))   # same name, other type
  expect_error(recipe_params(bad), "Parameter 'dx' is declared with different definitions")
  expect_length(recipe_params(omop_recipe()), 0L)
})

test_that("recipe_bind() type-checks, substitutes and rebuilds; recipe_to_plan() binds defaults", {
  rec <- .prm_recipe()
  expect_error(recipe_to_plan(rec), "Unbound parameter 'dx'")
  bound <- recipe_bind(rec, dx = omop_concept_set(201820, descendants = TRUE))
  expect_identical(recipe_to_plan(bound), .prm_bound_plan())
  expect_identical(names(recipe_params(bound)), c("cohort", "lookback"))   # defaults remain
  expect_identical(recipe_to_plan(recipe_bind(rec, dx = 201820, lookback = 30L, cohort = 7L)),
                   .prm_bound_plan(dx = 201820, lookback = 30L, cohort = 7L))
  # binding in stages equals binding at once
  staged <- recipe_bind(recipe_bind(rec, lookback = 30L), dx = 201820)
  expect_identical(recipe_to_plan(staged), recipe_to_plan(recipe_bind(rec, dx = 201820,
                                                                          lookback = 30L)))
  expect_error(recipe_bind(rec, nope = 1), "'nope' is not a parameter of the recipe")
  expect_error(recipe_bind(rec, 1), "by name")
  expect_error(recipe_bind(rec, lookback = 1.5), "exact integer")
  expect_error(recipe_bind(rec, lookback = NULL), "NULL leaves a parameter unbound")
  expect_error(recipe_bind(rec, dx = TRUE), "concept")
  # the bound value is validated where it is used: start after end
  expect_error(recipe_bind(rec, dx = 1, lookback = -5L), "start must not be after end")
  expect_identical(recipe_bind(rec), rec)
})

test_that("parameters may stand for any language-2 value and partial expressions keep bound operands", {
  a <- omop_param("a", type = "integer")
  b <- omop_param("b", type = "integer")
  rec <- omop_recipe(
    variables = list(n = omop_variable(event = omop_event("measurement", 3004410,
                                                          where = omop_where(value >= omop_param("t", type = "number"))),
                                       reducer = omop_reduce("count", window = omop_window(-(a + b), 0)))),
    outputs = omop_output("w", format = "wide"))
  half <- recipe_bind(rec, a = 300L)
  expect_setequal(names(recipe_params(half)), c("b", "t"))
  plan <- recipe_to_plan(recipe_bind(half, b = 65L, t = 6.5))
  ev <- plan$events[[plan$variables$n$source$event]]
  expect_identical(ev$where, list(var = "value", op = ">=", value = 6.5))
  expect_identical(plan$variables$n$reducer$window$start, -365L)
})

test_that("event, window, date and string parameters bind through their constructors", {
  ev <- omop_param("outcome", type = "event")
  win <- omop_param("tar", type = "window", default = omop_window(1, 365))
  rec <- omop_recipe(
    events = list(omop_event("condition_occurrence", 4329847, name = "mi")),
    populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1),
                                                                select = "all"))),
    outputs = list(omop_output("tte", rows = "c1", format = "survival", options = list(
      outcomes = list(o = ev), tar = win, censor = list("observation_end")))))
  plan <- recipe_to_plan(recipe_bind(rec, outcome = "mi"))
  expect_identical(plan$outputs$tte$options$outcomes$o, list(event = "mi"))
  expect_identical(plan$outputs$tte$options$tar$end, 365L)
  expect_error(recipe_bind(rec, outcome = 5), "omop_event")
  age <- omop_recipe(variables = list(age = omop_age(reference_date = omop_param("ref", type = "date"))),
                     outputs = omop_output("w", format = "wide"))
  expect_identical(recipe_to_plan(recipe_bind(age, ref = "2020-06-30"))$variables$age$derived$reference_date,
                   "2020-06-30")
})

test_that("parametrised recipes round-trip through JSON, YAML and recipe_to_code()", {
  rec <- .prm_recipe()
  target <- recipe_to_plan(recipe_bind(rec, dx = 201820))
  from_json <- recipe_import_json(recipe_export_json(rec))
  expect_setequal(names(recipe_params(from_json)), c("dx", "lookback", "cohort"))
  expect_identical(recipe_to_plan(recipe_bind(from_json, dx = 201820)), target)
  skip_if_not_installed("yaml")
  from_yaml <- recipe_import_yaml(recipe_export_yaml(rec))
  expect_identical(recipe_to_plan(recipe_bind(from_yaml, dx = 201820)), target)
  code <- recipe_to_code(rec)
  expect_match(code, "-omop_param(name = \"lookback\"", fixed = TRUE)
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = code), envir = env)
  expect_identical(recipe_to_plan(recipe_bind(env$recipe, dx = 201820)), target)
})

test_that("print() lists a recipe's parameters", {
  out <- capture.output(print(.prm_recipe()))
  line <- grep("^Parameters \\(3\\)", out, value = TRUE)
  expect_length(line, 1L)
  expect_match(line, "dx <concept_set> (unbound)", fixed = TRUE)
  expect_match(line, "lookback <integer> = 365", fixed = TRUE)
})
