# WS-9 simple layer (design-2.10.3 section 3, test plan 10.1): every
# simple-layer construct equals the advanced-layer recipe it expands to --
# the same plan (expect_identical) and so the same normalised semantic digest.

.smp_cs <- function() {
  list(t2dm = omop_concept_set(201826, descendants = TRUE, name = "t2dm"),
       metf = omop_concept_set(1503297, descendants = TRUE, name = "metformin"),
       mi = omop_concept_set(4329847, descendants = TRUE, name = "mi"))
}

.smp_same <- function(simple, advanced) {
  a <- recipe_to_plan(simple)
  b <- recipe_to_plan(advanced)
  expect_identical(a, b)
  expect_identical(dsOMOPClient:::.plan_v2_digest(a)$sha256, dsOMOPClient:::.plan_v2_digest(b)$sha256)
}

test_that("the section 3 study equals its hand-written advanced recipe", {
  cs <- .smp_cs()
  study <- omop_study(
    index    = omop_new_users(cs$metf, washout_days = 365),
    require  = list(prior_observation = 365, has = list(omop_event("condition_occurrence", cs$t2dm))),
    baseline = list(age = omop_age(), sex = omop_sex(),
                    hba1c = omop_value_at_index("measurement", 3004410, unit = 8554),
                    inpatient = omop_count("visit_occurrence", 9201, before = 365),
                    charlson = omop_score("charlson")),
    outcomes = list(mi = omop_event("condition_occurrence", cs$mi)),
    follow_up = list(start = 1, end = NULL, censor = c("observation_end", "death")))
  expect_s3_class(study, "omop_recipe")
  rx <- omop_event("drug_exposure", cs$metf)
  advanced <- omop_recipe(
    populations = list(omop_population("study", label = "New users",
      index = omop_index(omop_era(rx, by = "ingredient"), select = "first_eligible",
                         end = omop_end("interval_end")),
      filters = list(omop_observed(before = 365), omop_has_not(rx, window = omop_window(-365, -1)),
                     omop_observed(before = 365),
                     omop_has(omop_event("condition_occurrence", cs$t2dm), window = omop_window(NULL, 0))),
      coverage = omop_coverage())),
    variables = list(age = omop_age(), sex = omop_sex(),
      hba1c = omop_variable(event = omop_event("measurement", 3004410,
                                               where = omop_where(unit_concept_id == 8554)),
                            reducer = omop_reduce("closest_value", window = omop_window(-90, 30))),
      inpatient = omop_variable(event = omop_event("visit_occurrence", 9201),
                                reducer = omop_reduce("count", window = omop_window(-365, 0))),
      charlson = omop_score("charlson")),
    outputs = list(
      omop_output("baseline", rows = "study", format = "wide", grain = "episode"),
      omop_output("survival", rows = "study", format = "survival", grain = "episode",
                  options = list(outcomes = list(mi = omop_event("condition_occurrence", cs$mi)),
                                 tar = omop_window(1, NULL),
                                 censor = list("observation_end", "death"), format = "survival"))))
  .smp_same(study, advanced)
  expect_output(print(study), "outputs baseline, survival \\(default\\)")
  expect_output(print(study), "episode grain; survival ties follow the priority rule")
  # recipe_to_code() prints the expansion; it rebuilds the same plan
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(study)), envir = env)
  expect_identical(recipe_to_plan(env$recipe), recipe_to_plan(study))
  ex <- recipe_explain(study, detail = "semantics")
  expect_true("simple_layer" %in% ex$defaults$code)
  expect_true(any(grepl("dates are relative to the index date", ex$text, fixed = TRUE)))
  expect_true(any(grepl("ties priority", ex$text, fixed = TRUE)))
})

test_that("an event index with defaults expands to index/select/coverage/outputs as stated", {
  cs <- .smp_cs()
  dx <- omop_event("condition_occurrence", cs$t2dm, name = "t2dm_dx")
  death <- omop_event("death", name = "death")
  study <- omop_study(dx, baseline = list(age = omop_age()), outcomes = list(death = death))
  advanced <- omop_recipe(
    populations = list(omop_population("study", index = omop_index(dx, select = "first"),
                                       coverage = omop_coverage())),
    variables = list(age = omop_age()),
    outputs = list(
      omop_output("baseline", rows = "study", format = "wide", grain = "episode"),
      omop_output("survival", rows = "study", format = "survival", grain = "episode",
                  options = list(outcomes = list(death = death), tar = omop_window(1, NULL),
                                 censor = list("observation_end", "death"), format = "survival"))))
  .smp_same(study, advanced)
  out <- capture.output(print(study))
  expect_true(any(grepl("index selection \"first\" (default)", out, fixed = TRUE)))
  expect_true(any(grepl("follow-up [1, Inf] days from index (unbounded end)", out, fixed = TRUE)))
  expect_true(any(grepl("censoring at observation_end, death (default)", out, fixed = TRUE)))
  # a cohort index, other selections and coverage
  cohort <- omop_study(list(cohort_definition_id = 42), select = "all", outputs = "baseline",
                       baseline = list(sex = omop_sex()), name = "c42",
                       coverage = omop_coverage("earliest"))
  .smp_same(cohort, omop_recipe(
    populations = list(omop_population("c42", index = omop_index(list(cohort_definition_id = 42),
                                                                 select = "all"),
                                       coverage = omop_coverage("earliest"))),
    variables = list(sex = omop_sex()),
    outputs = list(omop_output("baseline", rows = "c42", format = "wide", grain = "episode"))))
  # an omop_index() is used as it is
  ix <- omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end"))
  expect_identical(omop_study(ix, baseline = list(sex = omop_sex()), outputs = "baseline")$populations$study$index,
                   ix)
})

test_that("require entries expand one criterion each, in the order given", {
  dx <- omop_event("condition_occurrence", 201826, name = "dx")
  bleed <- omop_event("condition_occurrence", 192671, name = "bleed")
  crit <- omop_has_not(omop_event("procedure_occurrence", 4032243), window = omop_window(-30, 0))
  study <- omop_study(dx, outputs = "baseline", baseline = list(sex = omop_sex()),
    require = list(prior_observation = 365, followup = 30, age = c(18, NA), sex = "F",
                   has = list(bleed), no_history_of = bleed, in_cohort = 7, crit,
                   omop_person(omop_where(year_of_birth >= 1940))))
  expected <- list(omop_observed(before = 365), omop_observed(after = 30), omop_age_is(min = 18),
                   omop_person(omop_where(gender_concept_id == 8532)),
                   omop_has(bleed, window = omop_window(NULL, 0)),
                   omop_has_not(bleed, window = omop_window(NULL, -1)), omop_in_cohort(7), crit,
                   omop_person(omop_where(year_of_birth >= 1940)))
  expect_identical(study$populations$study$filters, expected)
  expect_identical(omop_study(dx, outputs = "baseline", baseline = list(sex = omop_sex()),
                              require = list(sex = "m"))$populations$study$filters,
                   list(omop_person(omop_where(gender_concept_id == 8507))))
  expect_error(omop_study(dx, require = list(prior_obs = 1)), "unknown require entry 'prior_obs'")
  expect_error(omop_study(dx, require = list(age = 18)), "c\\(min, max\\)")
  expect_error(omop_study(dx, require = list(sex = "X")), "\"F\" or \"M\"")
  expect_error(omop_study(dx, require = list(omop_age())), "must be a criterion")
})

test_that("several outcomes give competing risks; long and panel have defined sources and bins", {
  mi <- omop_event("condition_occurrence", 4329847, name = "mi")
  death <- omop_event("death", name = "death")
  dx <- omop_event("condition_occurrence", 201826, name = "dx")
  study <- omop_study(dx, outcomes = list(mi = mi, death = death),
                      outputs = c("survival", "long", "panel"),
                      follow_up = list(end = 365, censor = "observation_end"))
  advanced <- omop_recipe(
    populations = list(omop_population("study", index = omop_index(dx, select = "first"),
                                       coverage = omop_coverage())),
    outputs = list(
      omop_output("survival", rows = "study", format = "survival", grain = "episode",
                  options = list(outcomes = list(mi = mi, death = death), tar = omop_window(1, 365),
                                 censor = list("observation_end"), format = "competing_risk")),
      omop_output("long", rows = "study", format = "long", grain = "episode", sources = list(mi, death)),
      omop_output("panel", rows = "study", format = "person_period", grain = "episode",
                  options = list(representation = "dense", bins = list(width = 30L),
                                 window = omop_window(1, 365), outcomes = list(mi = mi, death = death)))))
  .smp_same(study, advanced)
  for (o in c("survival", "long", "panel")) {
    expect_error(omop_study(dx, outputs = o), paste0("output \"", o, "\" needs outcomes"))
  }
  expect_error(omop_study(dx, outputs = "baseline"), "needs baseline variables")
  expect_error(omop_study(dx, outputs = "table"), "unknown output")
  expect_error(omop_study(dx, follow_up = list(stop = 1)), "unknown follow_up entry stop")
  expect_error(omop_study(dx, outcomes = list(omop_event("death"))), "name every outcome")
})

test_that("a population index keeps its own index and criteria and refuses a second selection", {
  cs <- .smp_cs()
  nu <- omop_new_users(cs$metf, washout_days = 180)
  study <- omop_study(nu, require = list(age = c(40, 80)), baseline = list(age = omop_age()),
                      outputs = "baseline", name = "users")
  pop <- study$populations$users
  expect_identical(pop$index, nu$index)
  expect_identical(pop$filters, c(nu$filters, list(omop_age_is(min = 40, max = 80))))
  expect_identical(pop$coverage, omop_coverage())
  expect_error(omop_study(nu, select = "all"), "keeps its own selection")
  expect_error(omop_study(omop_population("x", parent_id = "y", index = nu$index)), "stand alone")
  expect_error(omop_study(omop_population("x")), "has no index")
  own <- nu
  own$coverage <- omop_coverage("union", gap = 30L)
  expect_identical(omop_study(own, baseline = list(age = omop_age()),
                              outputs = "baseline")$populations$study$coverage, own$coverage)
  expect_error(omop_study(own, coverage = omop_coverage("earliest")), "its own coverage")
})

test_that("omop_new_users() is the era-indexed population with an observed washout", {
  cs <- .smp_cs()
  rx <- omop_event("drug_exposure", cs$metf)
  expect_identical(omop_new_users(cs$metf, washout_days = 365),
    omop_population("new_users", label = "New users",
      index = omop_index(omop_era(rx, by = "ingredient"), select = "first_eligible",
                         end = omop_end("interval_end")),
      filters = list(omop_observed(before = 365), omop_has_not(rx, window = omop_window(-365, -1)))))
  expect_identical(omop_new_users(1503297, washout_days = 0, select = "all", by = "event"),
    omop_population("new_users", label = "New users",
      index = omop_index(omop_era(omop_event("drug_exposure", 1503297), by = "event"),
                         select = "all", end = omop_end("interval_end"))))
  ev <- omop_event("drug_exposure", cs$metf, name = "metf_rx")
  expect_identical(omop_new_users(ev)$index$source$event, ev)
  expect_error(omop_new_users(omop_event("condition_occurrence", 1)), "drug_exposure")
  expect_error(omop_new_users(cs$metf, washout_days = -1), ">= 0")
})

test_that("each shortcut is the advanced variable it names", {
  t <- "condition_occurrence"
  adv <- function(ev, ...) omop_variable(event = ev, reducer = omop_reduce(...))
  ev <- omop_event(t, 201826)
  expect_identical(omop_ever(t, 201826, before = 365), adv(ev, "binary", window = omop_window(-365, 0)))
  expect_identical(omop_ever(t, 201826), adv(ev, "binary", window = omop_window(NULL, 0)))
  expect_identical(omop_ever(t, 201826, before = Inf), adv(ev, "binary", window = omop_window(NULL, 0)))
  expect_identical(omop_ever(t, 201826, after = 30), adv(ev, "binary", window = omop_window(1, 30)))
  expect_identical(omop_ever(t, 201826, after = Inf), adv(ev, "binary", window = omop_window(1, NULL)))
  expect_identical(omop_ever(t, 201826, window = omop_window(-10, 10)),
                   adv(ev, "binary", window = omop_window(-10, 10)))
  expect_identical(omop_ever(t, 201826, descendants = TRUE, visit = "index", before = 7),
                   adv(omop_event(t, 201826, descendants = TRUE, visit = "index"), "binary",
                       window = omop_window(-7, 0)))
  for (args in list(list(before = 1, after = 1), list(before = 1, window = omop_window(0, 1)),
                    list(after = 1, window = omop_window(0, 1)))) {
    expect_error(do.call(omop_ever, c(list(t, 201826), args)), "mutually exclusive")
    expect_error(do.call(omop_count, c(list(t, 201826), args)), "mutually exclusive")
    expect_error(do.call(omop_mean_value, c(list("measurement", 3004410), args)), "mutually exclusive")
  }
  expect_error(omop_ever(t, 201826, after = 0), ">= 1")
  expect_identical(omop_count(t, 201826, before = 365), adv(ev, "count", window = omop_window(-365, 0)))
  expect_identical(omop_count(t, 201826, before = 365, distinct = "day"),
                   adv(ev, "count", window = omop_window(-365, 0), distinct = "day"))
  lab <- omop_event("measurement", 3004410)
  lab_u <- omop_event("measurement", 3004410, where = omop_where(unit_concept_id == 8554))
  conv <- omop_unit(8554, convert = list("8753" = c(factor = 0.0915, offset = 2.15)))
  expect_identical(omop_last_value("measurement", 3004410, before = 180),
                   adv(lab, "last_value", window = omop_window(-180, 0)))
  expect_identical(omop_first_value("measurement", 3004410, unit = 8554),
                   adv(lab_u, "first_value", window = omop_window(NULL, 0)))
  expect_identical(omop_mean_value("measurement", 3004410, unit = conv, after = 90),
                   adv(omop_event("measurement", 3004410, unit = conv), "mean", window = omop_window(1, 90)))
  expect_identical(omop_min_value("measurement", 3004410, value = "range_low", before = 30),
                   adv(omop_event("measurement", 3004410, value = "range_low"), "min",
                       window = omop_window(-30, 0)))
  expect_identical(omop_max_value("measurement", 3004410, unit = c(8554, 8753)),
                   adv(omop_event("measurement", 3004410,
                                  where = omop_where(unit_concept_id %in% c(8554, 8753))),
                       "max", window = omop_window(NULL, 0)))
  expect_identical(omop_value_at_index("measurement", 3004410, unit = 8554),
                   adv(lab_u, "closest_value", window = omop_window(-90, 30)))
  expect_identical(omop_value_at_index("measurement", 3004410, tolerance = c(-30, 0)),
                   adv(lab, "closest_value", window = omop_window(-30, 0)))
  expect_identical(omop_days_since(t, 201826), adv(ev, "days_since", reference = "index"))
  expect_identical(omop_days_until(t, 201826), adv(ev, "days_until", reference = "index"))
  expect_identical(omop_first_date(t, 201826), adv(ev, "first_date"))
  expect_identical(omop_last_date(t, 201826), adv(ev, "last_date"))
  # an event object is used as it is
  named <- omop_event("measurement", 3004410, name = "hba1c")
  expect_identical(omop_last_value(named, before = 90), adv(named, "last_value", window = omop_window(-90, 0)))
  expect_error(omop_ever(named, 1), "carries its own concepts")
  expect_error(omop_value_at_index("measurement", 1, tolerance = 30), "c\\(start, end\\)")
})

test_that("omop_history() is one omop_ever() per item over the lookback", {
  h <- omop_history(condition = c(diabetes = 201820, 320128),
                    drug = list(statins = omop_concept_set(1539403, descendants = TRUE)),
                    procedure = 4032243, days_before = 180)
  expect_named(h, c("diabetes", "condition_320128", "statins", "procedure_4032243"))
  expect_identical(h$diabetes, omop_ever("condition_occurrence", 201820, before = 180))
  expect_identical(h$statins, omop_ever("drug_exposure", omop_concept_set(1539403, descendants = TRUE),
                                        before = 180))
  expect_identical(omop_history(condition = 201820, days_before = Inf)[[1L]],
                   omop_ever("condition_occurrence", 201820))
  expect_error(omop_history(), "give condition")
  # spliced into a study baseline, an outer name prefixes the inner names
  study <- omop_study(omop_event("condition_occurrence", 201826),
                      baseline = list(hx = omop_history(condition = c(dm = 201820)), age = omop_age()),
                      outputs = "baseline")
  expect_identical(names(study$variables), c("hx_dm", "age"))
  study2 <- omop_study(omop_event("condition_occurrence", 201826),
                       baseline = list(omop_history(condition = c(dm = 201820)), age = omop_age()),
                       outputs = "baseline")
  expect_identical(names(study2$variables), c("dm", "age"))
})

test_that("acceptance recipe 1 runs as an omop_study() baseline (Opal scenario O2)", {
  acc <- system.file("plans", "acceptance", package = "dsOMOP")
  if (!nzchar(acc)) acc <- testthat::test_path("..", "..", "..", "dsOMOP", "inst", "plans", "acceptance")
  skip_if(!dir.exists(acc), "dsOMOP acceptance corpus not installed")
  d <- file.path(acc, "01-characterisation")
  code <- readLines(file.path(d, "recipe.R"), warn = FALSE)
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = code[!grepl("^\\s*library\\(", code)]), envir = env)
  rec <- env$rec
  study <- omop_study(rec$populations$c1[["index"]], baseline = rec$variables, outputs = "baseline",
                      name = "c1")
  ours <- dsOMOPClient:::.planNormalizeV2(recipe_to_plan(study))
  frozen <- dsOMOPClient:::.planNormalizeV2(jsonlite::fromJSON(file.path(d, "plan.json"),
                                                               simplifyVector = FALSE))
  for (slot in c("concept_sets", "events", "populations", "variables")) {
    expect_identical(dsOMOPClient:::.jcsCanonical(ours[[slot]]),
                     dsOMOPClient:::.jcsCanonical(frozen[[slot]]), info = slot)
  }
  a <- ours$outputs$baseline
  b <- frozen$outputs$table1
  a$result_symbol <- b$result_symbol <- NULL
  expect_identical(dsOMOPClient:::.jcsCanonical(a), dsOMOPClient:::.jcsCanonical(b))
})
