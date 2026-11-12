# WS-8 (design-2.10.3 sections 2, 4.1, 4.4, 5): compiling recipes to v2 plans.

.wire <- function(plan) dsOMOPClient:::.v2_plain(plan)
.valid <- function(plan) {
  dsOMOPClient:::.jsonSchemaValidate(dsOMOPClient:::.v2_plan_schema(), .wire(plan))$valid
}

test_that("inline objects are interned under deterministic names; equal ones share a node", {
  dm <- omop_concept_set(201820, descendants = TRUE, name = "dm")
  r <- omop_recipe(
    populations = omop_population("p", index = omop_index(omop_event("condition_occurrence", dm)),
                                  filters = list(omop_has(omop_event("condition_occurrence", dm),
                                                          window = omop_window(-30, 0)))),
    variables = list(n = omop_variable(event = omop_event("condition_occurrence", dm),
                                       reducer = omop_reduce("count")),
                     lab = omop_variable(event = omop_event("measurement", 3004410),
                                         reducer = omop_reduce("mean"))),
    outputs = omop_output("o", rows = "p", format = "wide"))
  plan <- recipe_to_plan(r)
  expect_true(.valid(plan))
  expect_setequal(names(plan$events), c("dm_dx", "lab_3004410"))
  expect_identical(plan$events$dm_dx$concept_set, "dm")
  expect_identical(plan$concept_sets$dm$include_descendants, TRUE)
  expect_identical(plan$populations$p$index$source, list(event = "dm_dx"))
  # measurement events carry their role value column (value_as_number)
  expect_identical(plan$events$lab_3004410$value, list(column = "value_as_number"))
})

test_that("a name reused for a different definition is refused", {
  r <- omop_recipe(
    events = list(omop_event("condition_occurrence", 1, name = "x")),
    variables = list(v = omop_variable(event = omop_event("condition_occurrence", 2, name = "x"),
                                       reducer = omop_reduce("binary"))),
    outputs = omop_output("o", format = "wide"))
  expect_error(recipe_to_plan(r), "name 'x' is used for two different definitions")
  same <- omop_recipe(
    events = list(omop_event("condition_occurrence", 1, name = "x")),
    variables = list(v = omop_variable(event = omop_event("condition_occurrence", 1, name = "x"),
                                       reducer = omop_reduce("binary"))),
    outputs = omop_output("o", format = "wide"))
  expect_named(recipe_to_plan(same)$events, "x")
})

test_that("legacy population filters lower to native criteria; parent compiles (B-8)", {
  r <- omop_recipe(
    populations = list(
      omop_population("all_f", filters = list(omop_filter_sex("F"),
                                              omop_filter_age(18, 18, year = 2020))),
      omop_population("dm", parent_id = "all_f",
                      filters = list(omop_filter_has_concept(201820, "condition_occurrence",
                                                             min_count = 2),
                                     omop_filter_missing_measurement(3004410)))),
    variables = list(sex = omop_sex()),
    outputs = omop_output("o", rows = "dm", format = "wide"))
  plan <- recipe_to_plan(r)
  expect_true(.valid(plan))
  w <- plan$populations$all_f$where$and
  expect_identical(w[[1]], list(person = list(var = "gender_concept_id", op = "==", value = 8532L)))
  # a one-year band (F-26), annual age in the reference year
  expect_identical(w[[2]], list(age = list(min = 18L, max = 18L, reference = list(year = 2020L))))
  dm <- plan$populations$dm
  expect_identical(dm$parent, "all_f")
  expect_identical(dm$where$and[[1]]$has$n, 2L)
  # missing_measurement keeps its v1 meaning: no row with a non-null value (B-7)
  expect_identical(dm$where$and[[2]]$has$nonmissing, "value_as_number")
  expect_identical(dm$where$and[[2]]$has$op, "==")
  # the implicit base is not emitted when nothing uses it
  expect_false("base" %in% names(plan$populations))
  # the v1 rendering of the same 2.10.1-only recipe keeps the 2.10.1 plan
  # (parent was never compiled there; execution refuses it on old servers)
  legacy <- omop_recipe(populations = r$populations[c("all_f", "dm")],
                        variables = omop_variable_sex(),
                        outputs = omop_output("o", type = "wide", population_id = "dm"))
  v1 <- recipe_to_plan(legacy, version = 1)
  expect_s3_class(v1, "omop_plan")
  expect_null(v1$populations$dm$parent)
  expect_true(any(grepl("parent_id", dsOMOPClient:::.recipe_needs_v2(legacy))))
})

test_that("legacy index events and variables lower to their native forms", {
  r <- omop_recipe(
    populations = omop_population("t2d", index_event = omop_index_event(
      201820, "condition_occurrence", primary_limit = "first",
      end_strategy = list(DateOffset = list(DateField = "StartDate", Offset = 30)))),
    variables = list(omop_variable(name = "hba1c", table = "measurement", concept_id = 3004410,
                                   format = "last_value", time_window = list(start = -365, end = 0),
                                   value_source = "value_as_number"),
                     omop_variable_age(reference = "index")),
    outputs = omop_output("w", type = "features", population_id = "t2d", grain = "episode"))
  plan <- recipe_to_plan(r)
  expect_true(.valid(plan))
  ix <- plan$populations$t2d$index
  expect_identical(ix$select, "first")   # select-before-eligibility keeps its meaning
  expect_identical(ix$end, list(strategy = "fixed", days = 30L))
  expect_identical(plan$variables$hba1c$reducer$fn, "last_value")
  expect_identical(plan$variables$hba1c$reducer$window,
                   list(start = -365L, end = 0L, anchor = "index"))
  expect_identical(plan$variables$age$derived$kind, "age")
  expect_identical(plan$outputs$w$grain, "episode")
})

test_that("legacy constructs without a faithful native form are refused, naming them", {
  long <- omop_recipe(variables = omop_variable(table = "measurement", concept_id = 1),
                      outputs = omop_output("l", type = "long"))
  expect_error(recipe_to_plan(long), "legacy type 'long' has no v2 lowering")
  expect_s3_class(recipe_to_plan(long, version = 1), "omop_plan")
  score <- omop_recipe(variables = omop_variable_charlson(), outputs = omop_output("w"))
  expect_error(recipe_to_plan(score), "omop_score\\(\"charlson\"\\)")
  rowf <- omop_recipe(variables = omop_variable_sex(), filters = omop_filter_date_range("2020-01-01"),
                      outputs = omop_output("w"))
  expect_error(recipe_to_plan(rowf), "Recipe-level row filters")
})

test_that("version = 1 refuses language-2 constructs, naming them", {
  r <- omop_recipe(variables = list(age = omop_age()), outputs = omop_output("w", format = "wide"))
  expect_error(recipe_to_plan(r, version = 1), "language-2 construct.*variable 'age'.*format = \"wide\"")
  expect_error(recipe_to_plan(r, version = 3), "version must be 1 or 2")
})

test_that("grain defaults, collapse and population references are validated", {
  ev <- omop_event("condition_occurrence", 1, name = "dx")
  r <- omop_recipe(populations = omop_population("p", index = omop_index(ev)),
                   variables = list(sex = omop_sex()),
                   outputs = list(omop_output("e", rows = "p", format = "wide"),
                                  omop_output("c", rows = "p", format = "wide", grain = "person",
                                              collapse = "first_episode")))
  plan <- recipe_to_plan(r)
  expect_identical(plan$outputs$e$grain, "episode")
  expect_identical(plan$outputs$c$collapse, "first_episode")
  bad <- omop_recipe(populations = omop_population("p", index = omop_index(ev)),
                     variables = list(sex = omop_sex()),
                     outputs = omop_output("c", rows = "p", format = "wide", grain = "person"))
  expect_error(recipe_to_plan(bad), "needs collapse")
  ghost <- omop_recipe(variables = list(sex = omop_sex()),
                       outputs = omop_output("g", rows = "ghost", format = "wide"))
  expect_error(recipe_to_plan(ghost), "undeclared population")
  unknown_var <- omop_recipe(variables = list(sex = omop_sex()),
                             outputs = omop_output("g", format = "wide", columns = "age"))
  expect_error(recipe_to_plan(unknown_var), "unknown variable")
})

test_that("survival outputs: explicit censoring, A-7 tags, TAR default and the matrix", {
  a <- omop_era(omop_event("drug_era", 1, name = "a_rows"), name = "a")
  b <- omop_era(omop_event("drug_era", 2, name = "b_rows"), name = "b")
  mi <- omop_event("condition_occurrence", 4329847, name = "mi")
  pop <- omop_population("nu", index = omop_index(omop_any_of(a, b), "first_eligible"))
  surv <- function(...) omop_recipe(populations = pop,
                                    outputs = omop_output("t", rows = "nu", format = "survival",
                                                          options = list(...)))
  plan <- recipe_to_plan(surv(outcomes = list(mi = mi),
                              censor = list("observation_end",
                                            omop_censor(b, "first_start_after_entry",
                                                        when_index_source = "a"))))
  expect_true(.valid(plan))
  expect_identical(plan$outputs$t$options$tar, list(start = 0L, end = 730L, anchor = "index"))
  expect_identical(plan$outputs$t$options$censor[[2]]$censor$when_index_source, list("a"))
  expect_error(recipe_to_plan(surv(outcomes = list(mi = mi), censor = list(
    omop_censor(b, when_index_source = "c")))), "not index sources")
  expect_error(recipe_to_plan(surv(outcomes = list(mi = mi))), "explicit censor list")
  expect_error(recipe_to_plan(surv(outcomes = list(mi = mi), censor = "death",
                                   format = "competing_risk", event_order = "last")),
               "competing_risk requires")
  expect_error(recipe_to_plan(surv(outcomes = list(mi = mi), censor = "death", tie_policy = "error")),
               "disclosure oracle")
  expect_error(recipe_to_plan(surv(outcomes = list(mi = mi), censor = "death",
                                   tar = omop_window(1, NULL), tars = list(x = omop_window(1, 2)))),
               "tar or tars")
  expect_error(recipe_to_plan(surv(outcomes = list(mi = mi), censor = "death", bogus = 1)),
               "unsupported option")
})

test_that("person_period, effective supply and joined inputs are checked", {
  rx <- omop_event("drug_exposure", 1503297, end = list(fallback = "days_supply"), name = "rx")
  era <- omop_era(rx, name = "era")
  stock <- omop_era(rx, stockpile = TRUE, name = "stock")
  pop <- omop_population("c", index = omop_index(list(cohort_definition_id = 1), "all",
                                                end = omop_end("event_end")))
  ok <- omop_recipe(populations = pop,
                    variables = list(eff = omop_variable(event = stock, reducer = omop_reduce(
                      "covered_days", window = omop_window(1, 90), coverage = "effective_supply"))),
                    outputs = omop_output("w", rows = "c", format = "wide"))
  expect_true(.valid(recipe_to_plan(ok)))
  bad <- omop_recipe(populations = pop,
                     variables = list(eff = omop_variable(event = era, reducer = omop_reduce(
                       "covered_days", coverage = "effective_supply"))),
                     outputs = omop_output("w", rows = "c", format = "wide"))
  expect_error(recipe_to_plan(bad), "stockpiled")
  pp <- omop_recipe(populations = pop, outputs = omop_output("pp", rows = "c",
    format = "person_period", options = list(sources = list(rx), carry_forward = list(
      nope = list(lookback = 30)))))
  expect_error(recipe_to_plan(pp), "carry_forward key")
  j <- omop_recipe(populations = pop, variables = list(sex = omop_sex()),
                   outputs = list(omop_output("a", rows = "c", format = "wide"),
                                  omop_output("j", rows = "c", format = "joined",
                                              options = list(from = list("a", "zz")))))
  expect_error(recipe_to_plan(j), "joined input")
})

test_that("publication roots, scope and plan options are emitted", {
  ev <- omop_event("condition_occurrence", 1, name = "dx")
  r <- omop_recipe(populations = omop_population("p", index = omop_index(ev)),
                   variables = list(sex = omop_sex()),
                   outputs = list(omop_output("a", rows = "p", format = "wide"),
                                  omop_output("b", rows = "p", format = "wide")),
                   publish = "b", coverage = omop_coverage("earliest"),
                   options = list(translate_concepts = FALSE), cohort = 7)
  plan <- recipe_to_plan(r)
  expect_true(.valid(plan))
  expect_identical(plan$publish, list("b"))
  expect_identical(plan$options$coverage$policy, "earliest")
  expect_identical(plan$options$translate_concepts, FALSE)
  expect_identical(plan$scope$cohort, 7L)
  bad <- omop_recipe(variables = list(sex = omop_sex()), outputs = omop_output("a", format = "wide"),
                     publish = "zz")
  expect_error(recipe_to_plan(bad), "publish names unknown")
})

test_that("the operation inventory names what a plan needs (section 4.4)", {
  a <- omop_era(omop_event("drug_era", 1, name = "a_rows"), by = "ingredient", name = "a")
  b <- omop_era(omop_event("drug_era", 2, name = "b_rows"), name = "b")
  mi <- omop_event("condition_occurrence", 4329847, name = "mi")
  r <- omop_recipe(
    populations = list(
      omop_population("nu", index = omop_index(omop_any_of(a, b), "first_eligible"),
                      coverage = omop_coverage("union")),
      omop_population("child", parent_id = "nu", filters = list(omop_not(omop_has(mi))))),
    variables = list(arm = omop_population_column("index_source")),
    outputs = list(omop_output("w", rows = "child", format = "wide"),
                   omop_output("t", rows = "nu", format = "survival", options = list(
                     outcomes = list(mi = mi), censor = list(omop_censor(b, when_index_source = "a"))))),
    publish = c("w", "t"))
  ops <- dsOMOPClient:::.plan_v2_operations(recipe_to_plan(r))
  expect_true(all(c("population.index", "population.first_eligible", "population.any_of",
                    "population.coverage.union", "population.parent", "population.criteria",
                    "criteria.not", "interval", "interval.ingredient",
                    "variable.population_column", "output.wide", "output.survival",
                    "output.survival.censor_source", "event") %in% ops))
  expect_false("population.match" %in% ops)
  # as on the server, publish.roots is needed only for a strict subset of outputs
  expect_false("publish.roots" %in% ops)
  r$publish <- "w"
  expect_true("publish.roots" %in% dsOMOPClient:::.plan_v2_operations(recipe_to_plan(r)))
})

test_that("plans are sent losslessly (no 4-digit rounding)", {
  w <- omop_event("measurement", 3025315,
                  unit = omop_unit(9529, list("8739" = c(factor = 0.45359237))), name = "w")
  r <- omop_recipe(variables = list(x = omop_variable(event = w, reducer = omop_reduce("mean"))),
                   outputs = omop_output("o", format = "wide"))
  plan <- recipe_to_plan(r)
  enc <- dsOMOPClient:::.ds_encode_lossless(plan)
  b64 <- chartr("-_", "+/", sub("^B64:", "", enc))
  b64 <- paste0(b64, strrep("=", (4 - nchar(b64) %% 4) %% 4))
  back <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)), simplifyVector = FALSE)
  expect_identical(back$events$w$unit$convert[["8739"]]$factor, 0.45359237)
  expect_identical(dsOMOPClient:::.plan_v2_digest(back)$sha256,
                   dsOMOPClient:::.plan_v2_digest(plan)$sha256)
})

test_that("v2 plans save and load losslessly (JSON and YAML)", {
  w <- omop_event("measurement", 3025315,
                  unit = omop_unit(9529, list("8739" = c(factor = 0.45359237))), name = "w")
  r <- omop_recipe(
    populations = omop_population("c", index = omop_index(list(cohort_definition_id = 1), "all",
                                                         end = omop_end("event_end"))),
    variables = list(x = omop_variable(event = w, reducer = omop_reduce("mean"))),
    outputs = list(omop_output("o", rows = "c", format = "wide"),
                   omop_output("pp", rows = "c", format = "person_period",
                               options = list(sources = list(w), partition = list()))))
  plan <- recipe_to_plan(r)
  for (ext in c("json", "yaml")) {
    f <- withr::local_tempfile(fileext = paste0(".", ext))
    ds.omop.plan.save(plan, f)
    back <- ds.omop.plan.load(f)
    expect_s3_class(back, "omop_plan_v2")
    expect_identical(dsOMOPClient:::.plan_v2_digest(back)$sha256,
                     dsOMOPClient:::.plan_v2_digest(plan)$sha256, info = ext)
  }
})

test_that("emitted v2 plans never contain a compatibility (IR-only) construct", {
  r <- omop_recipe(
    populations = list(omop_population("a", index_event = omop_index_event(201820, "condition_occurrence")),
                       omop_population("b", parent_id = "a",
                                       filters = list(omop_filter_has_concept(1, "drug_exposure"),
                                                      omop_filter_age(18, 65)))),
    variables = list(omop_variable_age(reference = "index"), sex = omop_sex()),
    outputs = omop_output("w", type = "wide", population_id = "b"))
  json <- as.character(jsonlite::toJSON(dsOMOPClient:::.v2_plain(recipe_to_plan(r)),
                                        auto_unbox = TRUE, null = "null"))
  for (k in c("\"legacy\"", "legacy_spec", "legacy_end_strategy", "legacy_cohort_end_cap",
              "legacy_composite", "legacy_covariate_ids", "legacy_split",
              "legacy_all_time_conditions", "filter_tree", "\"type\":\"person_level\"")) {
    expect_false(grepl(k, json, fixed = TRUE), info = k)
  }
})

test_that("language-2 recipes round-trip through JSON, YAML and generated code", {
  rx <- omop_event("drug_exposure", omop_concept_set(1503297, descendants = TRUE, name = "metf"),
                   end = list(fallback = "days_supply"), name = "rx")
  era <- omop_era(rx, gap = 15, stockpile = TRUE, name = "era")
  mi <- omop_event("condition_occurrence", 4329847, descendants = TRUE, name = "mi")
  r <- omop_recipe(
    events = list(rx, mi), intervals = list(era), coverage = omop_coverage("earliest"),
    populations = list(
      omop_population("nu", label = "New users",
                      index = omop_index(era, "first_eligible", end = omop_end("interval_end", pad = 7)),
                      filters = list(omop_observed(before = 365, gap = 30),
                                     omop_has_not(rx, window = omop_window(-365, -1)),
                                     omop_any(omop_person(omop_where(year_of_birth >= 1950)),
                                              omop_age_is(40, NULL)))),
      omop_population("eligible", filters = list(omop_observed(before = 365))),
      omop_population("cc", match = omop_match("nu", "eligible", mi,
                                               match = list("sex", omop_age_band(10),
                                                            omop_strata(mi, before = 365)),
                                               seed = 5))),
    variables = list(age = omop_age(bands = TRUE, age_breaks = c(0, 18, 65)),
                     pdc = omop_variable(event = era, reducer = omop_reduce(
                       "covered_days", window = omop_window(1, 90),
                       coverage = "effective_supply", observed_only = TRUE)),
                     a1c = omop_variable(event = omop_event("measurement", 3004410,
                                                            unit = omop_unit(8554, list("8753" = c(factor = 0.0915, offset = 2.15))),
                                                            where = omop_where(value > 0), name = "a1c"),
                                         reducer = omop_reduce("closest_value", side = "before",
                                                               window = omop_window(-180, 0))),
                     set = omop_population_column("set_id")),
    outputs = list(omop_output("w", rows = "cc", format = "wide"),
                   omop_output("t", rows = "nu", format = "survival", options = list(
                     outcomes = list(mi = mi), tars = list(itt = omop_window(1, NULL)),
                     censor = list("observation_end", list(admin_date = "2025-12-31"),
                                   omop_censor(era, offset = 30, missing = "ineligible"))))),
    publish = c("w", "t"))
  d0 <- dsOMOPClient:::.plan_v2_digest(recipe_to_plan(r))$sha256
  expect_identical(dsOMOPClient:::.plan_v2_digest(
    recipe_to_plan(recipe_import_json(recipe_export_json(r))))$sha256, d0)
  expect_identical(dsOMOPClient:::.plan_v2_digest(
    recipe_to_plan(recipe_import_yaml(recipe_export_yaml(r))))$sha256, d0)
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(r)), envir = env)
  expect_identical(dsOMOPClient:::.plan_v2_digest(recipe_to_plan(env$recipe))$sha256, d0)
})

test_that("concept-set registries exchange OHDSI concept-set JSON", {
  r <- omop_recipe(concept_sets = list(
    t2dm = omop_concept_set(201826, descendants = TRUE,
                            exclude = omop_concept_set(195771, descendants = TRUE)),
    statins = omop_concept_set(c(1539403, 1545958))))
  expect_named(recipe_concept_sets(r), c("t2dm", "statins"))
  json <- recipe_export_concept_sets(r)
  back <- recipe_import_concept_sets(omop_recipe(), json)
  expect_identical(recipe_concept_sets(back)$t2dm$exclude$concepts, 195771L)
  expect_true(recipe_concept_sets(back)$t2dm$exclude$descendants)
  expect_identical(recipe_concept_sets(back)$statins$concepts, c(1539403L, 1545958L))
  expect_error(omop_recipe(concept_sets = list(a = omop_concept_set(1, name = "b"))),
               "use one name")
  mixed <- '{"items": [{"concept": {"CONCEPT_ID": 1}, "includeDescendants": true},
                       {"concept": {"CONCEPT_ID": 2}, "includeDescendants": false}]}'
  expect_error(recipe_import_concept_sets(omop_recipe(), mixed), "same for every")
  expect_output(print(r), "Concept sets \\(2\\): t2dm, statins")
})

test_that("DP preparation of a language-2 recipe sends the negotiated v2 plan", {
  state <- .publication_backend(list(srv1 = .publication_status(c(w = "D_w"))))
  .with_caps(list(srv1 = .caps(c("concept_set", "event", "variable.reducer", "variable.derived",
                                 "output.wide", "population.criteria"), reducers = "binary")))
  log <- .capture_exec()
  r <- omop_recipe(variables = list(x = omop_variable(event = omop_event("drug_exposure", 1),
                                                      reducer = omop_reduce("binary"))),
                   outputs = omop_output("w", format = "wide"))
  out <- ds.omop.dp.prepare(r)
  expect_identical(as.character(log$exec[[1L]]), "omopDpPlanExecuteDS")
  expect_identical(.decode(log$exec[[3L]])$version, 2L)
  expect_identical(as.list(log$exec)$commit_policy, "server")
  expect_identical(out, c(w = "D_w"))
})

test_that("an index-relative age criterion needs an index; explicit names are reserved", {
  r <- omop_recipe(populations = omop_population("adults", filters = list(omop_age_is(18, NULL))),
                   variables = list(sex = omop_sex()),
                   outputs = omop_output("w", rows = "adults", format = "wide"))
  expect_error(recipe_to_plan(r), "index-referenced age criterion needs an indexed population")
  ok <- omop_recipe(populations = omop_population("adults", filters = list(omop_age_is(18, year = 2024))),
                    variables = list(sex = omop_sex()),
                    outputs = omop_output("w", rows = "adults", format = "wide"))
  expect_s3_class(recipe_to_plan(ok), "omop_plan_v2")
  # a generated name never takes a name the user gave elsewhere
  dm <- omop_concept_set(1, name = "dm")
  r2 <- omop_recipe(
    populations = omop_population("p", index = omop_index(omop_event("condition_occurrence", dm))),
    variables = list(v = omop_variable(event = omop_event("condition_occurrence", 2, name = "dm_dx"),
                                       reducer = omop_reduce("binary"))),
    outputs = omop_output("w", rows = "p", format = "wide"))
  plan <- recipe_to_plan(r2)
  expect_identical(plan$events$dm_dx$concept_set$concepts, list(2L))
  expect_identical(plan$populations$p$index$source$event, "dm_dx_2")
})

test_that("each language-2 object has its section 4 JSON form", {
  dm <- omop_concept_set(201820, descendants = TRUE, name = "dm")
  ev <- omop_event("condition_occurrence", dm, name = "dx")
  w <- dsOMOPClient:::.v2_wire_of(omop_has(ev, n = 2, window = omop_window(-365, 0), distinct = "day"))
  expect_identical(w$node$has$source, list(event = "dx"))
  expect_named(w$events, "dx")
  expect_named(w$concept_sets, "dm")
  expect_true(dsOMOPClient:::.jsonSchemaValidate(dsOMOPClient:::.v2_schema_def("criteria"), w$node,
                                                 root = dsOMOPClient:::.v2_plan_schema())$valid)
  expect_identical(dsOMOPClient:::.v2_wire_of(omop_window(omop_bound("index", 1), NULL))$node,
                   list(start = list(anchor = "index", offset = 1L), end = NULL, anchor = "index"))
  expect_identical(dsOMOPClient:::.v2_wire_of(omop_reduce("count", distinct = "visit"))$node,
                   list(fn = "count", distinct = "visit"))
  expect_error(dsOMOPClient:::.v2_wire_of(omop_output("o")), "compiled with their recipe")
})

.windowed_features <- function(grain = NULL, type = "features", indexed = TRUE) {
  vars <- list(omop_variable(name = "a", table = "measurement", concept_id = 1, format = "count",
                             time_window = list(start = -90, end = 0)))
  pops <- if (indexed) omop_population("p", index_event = omop_index_event(201820,
                                                                           "condition_occurrence"))
  omop_recipe(populations = pops, variables = vars,
              outputs = omop_output(name = "f", type = type, population_id = if (indexed) "p" else "base",
                                    options = if (!is.null(grain)) list(grain = grain) else list()))
}

test_that("the features preset takes episode grain when windowed (design 2.9, A-14, B5)", {
  # omitted grain, indexed population: the newly permitted v2 rendering, which
  # the server's semantic validation accepts
  r <- .windowed_features()
  plan <- recipe_to_plan(r)
  expect_identical(plan$outputs$f$grain, "episode")
  e <- tryCatch(recipe_to_plan(r, version = 1), error = function(e) e)
  expect_s3_class(e, "dsomop_v1_newly_permitted")
  expect_s3_class(dsOMOPClient:::.recipe_plan_auto(r), "omop_plan_v2")
  skip_if_not_installed("dsOMOP")
  expect_length(dsOMOP:::.irSemanticErrors(dsOMOP:::.planNormalizeV2(dsOMOPClient:::.v2_plain(plan))), 0L)
})

test_that("only an omitted grain is newly permitted; an explicit one keeps the v1 refusal (B5)", {
  person <- .windowed_features(grain = "person")
  e <- tryCatch(recipe_to_plan(person, version = 1), error = function(e) e)
  expect_s3_class(e, "error")
  expect_false(inherits(e, "dsomop_v1_newly_permitted"))
  expect_error(dsOMOPClient:::.recipe_plan_auto(person), "grain")
  episode <- .windowed_features(grain = "episode")
  expect_s3_class(dsOMOPClient:::.recipe_plan_auto(episode), "omop_plan")   # v1 compiles
  wide <- .windowed_features(type = "wide")
  e <- tryCatch(recipe_to_plan(wide, version = 1), error = function(e) e)
  expect_false(inherits(e, "dsomop_v1_newly_permitted"))
  expect_error(dsOMOPClient:::.recipe_plan_auto(wide), "grain")
})

test_that("episode grain over an unindexed population is refused plainly (B5)", {
  r <- .windowed_features(indexed = FALSE)
  expect_error(recipe_to_plan(r), "episode grain needs an indexed population")
  expect_error(dsOMOPClient:::.recipe_plan_auto(r),
               "episode grain needs an indexed population.*cannot execute as a v1 plan either")
  flat <- omop_recipe(variables = list(omop_variable(name = "b", table = "measurement",
                                                     concept_id = 2, format = "count")),
                      outputs = omop_output(name = "f", type = "features"))
  expect_identical(recipe_to_plan(flat)$outputs$f$grain, "person")
})
