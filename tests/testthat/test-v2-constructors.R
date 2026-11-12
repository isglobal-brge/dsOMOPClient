# WS-8 (design-2.10.3 section 2): structural validation of the language-2
# constructors.

test_that("concept sets: ids, flags, exclusion forms and names", {
  cs <- omop_concept_set(c(201826, 201826, 1), descendants = TRUE, name = "t2dm")
  expect_s3_class(cs, "omop_concept_set")
  expect_identical(cs$concepts, c(201826L, 1L))
  expect_identical(omop_concept_set(integer(0))$concepts, integer(0))   # state none
  expect_error(omop_concept_set(-1), "non-negative")
  expect_error(omop_concept_set(1.5), "non-negative")
  expect_error(omop_concept_set(1, name = "1bad"), "name")
  ex <- omop_concept_set(1, exclude = omop_concept_set(2, descendants = TRUE))
  expect_true(ex$exclude$descendants)
  expect_error(omop_concept_set(1, exclude = omop_concept_set(2, name = "x")),
               "expanded inline")
  expect_identical(omop_concept_set(1, exclude = c(2, 3))$exclude, c(2L, 3L))
})

test_that("events: tables, concept forms, adapters and closed options", {
  e <- omop_event("Measurement", c(3004410, 3004411), descendants = TRUE, name = "lab")
  expect_identical(e$table, "measurement")
  expect_s3_class(e$concepts, "omop_concept_set")
  expect_true(e$concepts$descendants)
  expect_error(omop_event("cost"), "not an event source")
  expect_error(omop_event("episode_event"), "not an event source")
  expect_error(omop_event("measurement", omop_concept_set(1), descendants = TRUE),
               "on the concept set itself")
  expect_error(omop_event("measurement", "cs_name", descendants = TRUE), "registry")
  expect_error(omop_event("measurement", visit = "nope"), "visit")
  expect_identical(omop_event("measurement", visit = list(type = 9201))$visit,
                   list(type = 9201L))
  expect_error(omop_event("drug_exposure", end = list(fallback = "other")), "end must be")
  expect_identical(omop_event("death", on_missing_table = "unavailable")$on_missing_table,
                   "unavailable")
  expect_error(omop_event("death", on_missing_table = "skip"), "on_missing_table")
  ep <- omop_event("episode", 32531, name = "episodes")
  m <- omop_episode_members(ep, tables = "drug_exposure", name = "members")
  expect_identical(m$kind, "episode_members")
  expect_error(omop_episode_members(omop_event("condition_occurrence")), "episode table")
  cost <- omop_cost(omop_event("visit_occurrence"), field = "total_paid", currency = 44818668)
  expect_identical(cost$field, "total_paid")
  expect_error(omop_cost(omop_event("visit_occurrence"), field = "price"), "field")
  expect_error(omop_cost(m), "table event")
})

test_that("omop_where captures the closed grammar as data", {
  expect_identical(omop_where(value >= 6.5)$tree, list(var = "value", op = ">=", value = 6.5))
  expect_identical(omop_where(6.5 <= value)$tree, list(var = "value", op = ">=", value = 6.5))
  expect_identical(omop_where(!is.na(value))$tree, list(var = "value", op = "not_null"))
  expect_identical(omop_where(is.na(unit_concept_id))$tree,
                   list(var = "unit_concept_id", op = "is_null"))
  expect_identical(omop_where(x %in% c(1, 2))$tree,
                   list(var = "x", op = "in", value = list(1, 2)))
  expect_identical(omop_where(!(x %in% c(1, 2)))$tree$op, "not_in")
  t <- omop_where(a > 1 & b < -2 & c == "z")$tree
  expect_identical(names(t), "and")
  expect_length(t$and, 3L)
  expect_identical(t$and[[2]]$value, -2)
  expect_identical(names(omop_where(a > 1 | b < 2)$tree), "or")
  expect_identical(names(omop_where(!(a > 1))$tree), "not")
  expect_identical(names(omop_where(a > 1, b < 2)$tree), "and")
  expect_error(omop_where(value), "bare column")
  expect_error(omop_where(value > f(1)), "never evaluated")
  expect_error(omop_where(sqrt(value) > 1), "comparison needs one column|literal")
  expect_error(omop_where(a > 1 && b < 2), "use & and \\|")
  expect_error(omop_where(Value > 1), "lower-case")
  expect_error(omop_where(value = 1), "unnamed")
  tr <- omop_where_tree(list(var = "value", op = "between", value = list(lower = 5, upper = 10)))
  expect_s3_class(tr, "omop_predicate")
  expect_error(omop_where_tree(list(var = "value", op = "between",
                                    value = list(lower = 10, upper = 5))), "ordered")
  expect_error(omop_where_tree(list(var = "value", op = "<", value = list(1, 2))), "Invalid")
})

test_that("units: target, finite positive factors, unknown and censored policies", {
  u <- omop_unit(8554, convert = list("8753" = c(factor = 0.0915, offset = 2.15)))
  expect_identical(names(u$convert), "8753")
  expect_identical(u$convert[["8753"]], c(factor = 0.0915, offset = 2.15))
  expect_identical(omop_unit(8840, list("8753" = c(factor = 18)))$convert[["8753"]][["offset"]], 0)
  expect_error(omop_unit(8554, list("8753" = c(factor = 0))), "> 0")
  expect_error(omop_unit(8554, list("8753" = c(factor = Inf))), "finite")
  expect_error(omop_unit(8554, list(mg = c(factor = 1))), "unit concept IDs")
  expect_error(omop_unit(8554, unknown = "drop"), "unknown")
  expect_identical(omop_unit(8554, censored = "exclude")$censored, "exclude")
})

test_that("criteria: quantifiers, logic, person, age, observation and cohorts", {
  ev <- omop_event("condition_occurrence", 201820, name = "dm")
  h <- omop_has(ev, window = omop_window(-365, 0))
  expect_s3_class(h, "omop_criterion")
  expect_identical(h$op, ">=")
  expect_identical(h$n, 1L)
  expect_identical(h$distinct, "row")
  expect_identical(omop_count_is(ev, "between", c(2, 5))$n, c(2L, 5L))
  expect_error(omop_count_is(ev, "between", 2), "n = c\\(min, max\\)")
  expect_error(omop_count_is(ev, "between", c(5, 2)), "ordered")
  expect_error(omop_count_is(ev, ">", 2), "op")
  expect_error(omop_has(ev, n = -1), ">= 0")
  expect_identical(omop_has_not(ev, nonmissing = "value_as_number")$nonmissing, "value_as_number")
  expect_identical(omop_not(h)$kind, "not")
  expect_length(omop_all(h, omop_observed(before = 365))$criteria, 2L)
  expect_error(omop_all(), "at least one")
  expect_error(omop_any(1), "expected a criterion")
  expect_identical(omop_person(omop_where(year_of_birth >= 1950))$kind, "person")
  expect_identical(omop_age_is(18, 18)$min, 18L)
  expect_error(omop_age_is(), "min and/or max")
  expect_error(omop_age_is(65, 18), "must not exceed")
  expect_identical(omop_age_is(40, year = 2020)$year, 2020L)
  expect_identical(omop_observed(before = 365, gap = 30)$gap, 30L)
  expect_error(omop_observed(gap = -1), ">= 0")
  expect_identical(omop_in_cohort(42)$cohort_definition_id, 42L)
})

test_that("windows: -Inf/Inf are unbounded only at their own end (A-E5)", {
  w <- omop_window(-Inf, Inf)
  expect_null(w$start)
  expect_null(w$end)
  expect_identical(omop_window(-365, 0)$start, -365L)
  expect_error(omop_window(Inf, 0), "not an unbounded start")
  expect_error(omop_window(0, -Inf), "not an unbounded end")
  expect_error(omop_window(10, 5), "start must not be after end")
  expect_error(omop_window(1.5, 3), "exact integer")
  expect_identical(omop_window(0, 30, anchor = "2020-01-01")$anchor, "2020-01-01")
  expect_error(omop_window(0, 30, anchor = "2020-02-30"), "valid calendar date")
  expect_error(omop_window(0, 30, anchor = "tomorrow"), "anchor")
  b <- omop_bound("index_end", 30)
  expect_identical(b$offset, 30L)
  a <- omop_anchor(omop_event("drug_exposure", 1, name = "rx"), "latest_end", field = "end")
  expect_identical(a$which, "latest_end")
  expect_error(omop_anchor(omop_event("drug_exposure", 1), "latest_end"), "field = \"end\"")
  expect_error(omop_anchor(omop_event("drug_exposure", 1), "first", n = 2), "nth")
  # A-E5 on the 2.10.1 helpers too
  expect_identical(dsOMOPClient:::.normalize_recipe_time_window(list(start = -Inf, end = 0), "v"),
                   list(end = 0L))
  expect_error(dsOMOPClient:::.normalize_recipe_time_window(list(start = Inf, end = 0), "v"),
               "not an unbounded start")
  expect_error(dsOMOPClient:::.normalize_recipe_time_window(list(start = -Inf, end = Inf), "v"),
               "unbounded on both sides")
})

test_that("index, any_of, end strategies and coverage", {
  era <- omop_era(omop_event("drug_exposure", 1503297, name = "metf"), name = "metf_era")
  i <- omop_index(era, "first_eligible", end = omop_end("interval_end"))
  expect_identical(i$select, "first_eligible")
  expect_error(omop_index(era, "first", n = 2), "nth")
  expect_identical(omop_index(list(cohort_definition_id = 3))$source$cohort_definition_id, 3L)
  expect_error(omop_index(list(cohort_definition_id = 0)), ">= 1")
  expect_error(omop_index(era, end = "event_end"), "omop_end")
  a <- omop_event("drug_era", 1, name = "a"); b <- omop_event("drug_era", 2, name = "b")
  expect_length(omop_any_of(a, b)$arms, 2L)
  expect_identical(vapply(omop_any_of(a, b, order = c("b", "a"))$arms, `[[`, "", "name"),
                   c("b", "a"))
  expect_error(omop_any_of(a), "at least two")
  expect_error(omop_end("fixed"), "needs days")
  expect_error(omop_end("event_end", days = 3), "only with strategy")
  expect_error(omop_end("event_end", pad = -1), ">= 0")
  cov <- omop_coverage("union", gap = 30)
  expect_identical(cov$policy, "union")
  expect_identical(cov$on_missing, "drop")
  expect_error(omop_coverage("all"), "policy")
})

test_that("eras and censoring (A-7 when_index_source; default which)", {
  rx <- omop_event("drug_exposure", 1, name = "rx")
  e <- omop_era(rx)
  expect_identical(e$gap, 30L)
  expect_identical(e$fallback, "days_supply")
  expect_error(omop_era(e), "an event")
  expect_error(omop_era(rx, by = "atc"), "by")
  c1 <- omop_censor(e)
  c2 <- omop_censor(e, which = "first_end_after_entry")
  expect_identical(c1, c2)   # an omitted which equals the explicit default
  c3 <- omop_censor(e, "first_start_after_entry", when_index_source = c("acei", "arb"))
  expect_identical(c3$when_index_source, c("acei", "arb"))
  expect_error(omop_censor(e, when_index_source = c("a", "a")), "duplicates")
  expect_error(omop_censor(e, when_index_source = character(0)), "non-empty")
  expect_error(omop_censor(e, missing = "drop"), "missing")
})

test_that("reducers follow the frozen contract's shape rules", {
  expect_identical(omop_reduce("last_value")$value, "value")
  expect_null(omop_reduce("binary")$value)
  expect_identical(omop_reduce("nth_value", n = 1:3)$n, 1:3)
  expect_error(omop_reduce("mean", n = 1:2), "nth_value")
  expect_error(omop_reduce("quantile"), "needs p")
  expect_error(omop_reduce("quantile", p = 0), "\\(0, 1\\]")
  expect_error(omop_reduce("any"), "needs a predicate")
  expect_identical(omop_reduce("all", predicate = omop_where(value < 7))$fn, "all")
  expect_error(omop_reduce("days_since"), "reference anchor")
  expect_error(omop_reduce("time_since"), "reference_date")
  expect_error(omop_reduce("count", coverage = "effective_supply"), "covered_days")
  expect_error(omop_reduce("count", observed_only = TRUE), "covered_days")
  expect_error(omop_reduce("count", fills = "clipped"), "supply_days")
  expect_identical(omop_reduce("supply_days", fills = "clipped")$fills, "clipped")
  expect_error(omop_reduce("mean", side = "before"), "closest_value")
  expect_error(omop_reduce("mean", distinct = "day"), "count/n_distinct")
  expect_identical(omop_reduce("count", distinct = "visit")$distinct, "visit")
  expect_error(omop_reduce("median_value"), "fn")
})

test_that("variables: the two spellings, helpers, names", {
  ev <- omop_event("measurement", 3004410, name = "hba1c")
  v <- omop_variable(name = "x", event = ev, reducer = omop_reduce("mean"))
  expect_identical(v$kind, "reducer")
  expect_error(omop_variable(name = "x", table = "measurement", event = ev,
                             reducer = omop_reduce("mean")), "two spellings")
  expect_error(omop_variable(event = ev), "both event and")
  expect_error(omop_variable(name = "x"), "table is required")
  expect_error(omop_variable(table = "person", visit = "index", visit_filter = list(concept_ids = 1)),
               "alias")
  legacy <- omop_variable(table = "measurement", concept_id = 1, format = "count",
                          visit = list(type = 9201))
  expect_identical(legacy$visit_filter, list(concept_ids = 9201L))
  expect_identical(omop_age()$derived$reference, "index")
  expect_identical(omop_age(year = 2024)$derived$reference_date, "2024-07-01")
  expect_identical(omop_age(bands = TRUE, age_breaks = c(0, 18, 19, 65))$derived$age_breaks,
                   c(0L, 18L, 19L, 65L))   # any strictly increasing grid from 0 (F-26)
  expect_error(omop_age(bands = TRUE, age_breaks = c(5, 18)), "start at 0")
  expect_error(omop_age(age_breaks = c(0, 18)), "bands = TRUE")
  expect_identical(omop_score("charlson")$derived$kind, "charlson")
  expect_error(omop_score("elixhauser"), "type")
  expect_error(omop_prior_obs(), "reference_date is required")
  expect_identical(omop_observed_days(omop_window(-365, 0))$derived$kind, "observed_days")
  expect_identical(omop_population_column("set_id")$population_column, "set_id")
  expect_identical(omop_birth_year_band(10)$derived$width, 10L)
  r <- omop_recipe(variables = list(omop_age(), age_band = omop_age(bands = TRUE), omop_sex()))
  expect_identical(names(r$variables), c("age", "age_band", "sex"))
  expect_error(omop_recipe(variables = list(other = omop_variable(name = "x", event = ev,
                                                                  reducer = omop_reduce("mean")))),
               "use one name")
})

test_that("matching: inputs, items and closed policies", {
  mi <- omop_event("condition_occurrence", 4329847, name = "mi")
  m <- omop_match("cases", "eligible", mi, match = list("sex", omop_age_band(5),
                                                       omop_strata(mi, before = 365)),
                  seed = 7)
  expect_identical(m$seed, 7L)
  expect_identical(m$match[[3]]$window$start, -365L)
  expect_error(omop_match("cases", "eligible", mi, match = list("region")), "match items")
  expect_error(omop_match("cases", "eligible", mi, seed = -1), ">= 0")
  expect_error(omop_match("cases", "eligible", mi, replacement = "with"), "replacement")
  expect_error(omop_strata(mi, before = 3, window = omop_window(0, 1)), "not both")
})

test_that("populations and outputs accept the appended arguments consistently", {
  ev <- omop_event("condition_occurrence", 201820, name = "dm")
  p <- omop_population("p", index = omop_index(ev), coverage = omop_coverage("earliest"))
  expect_identical(p$coverage$policy, "earliest")
  expect_error(omop_population("p", index = omop_index(ev),
                               index_event = omop_index_event(1, "condition_occurrence")),
               "not both")
  expect_error(omop_population("p", index = omop_index(ev), cohort_definition_id = 1),
               "omop_index\\(list\\(cohort_definition_id")
  expect_error(omop_population("p", union = c("a", "b"), coverage = omop_coverage()),
               "set-op")
  expect_error(omop_population("p", match = omop_match("a", "b", ev), filters = list(omop_observed())),
               "omop_match\\(\\) alone")
  expect_error(omop_output("o", type = "wide", format = "wide"), "alternatives")
  expect_error(omop_output("o", population_id = "a", rows = "b"), "alias")
  expect_error(omop_output("o", variables = "a", columns = "b"), "alias")
  expect_error(omop_output("o", sources = list(ev)), "format")
  expect_error(omop_output("o", format = "parquet"), "format")
  expect_identical(omop_output("o", type = "features", grain = "episode")$options$grain, "episode")
  o <- omop_output("o", rows = "p", format = "wide", grain = "person", collapse = "any")
  expect_identical(o$population_id, "p")
  expect_identical(o$collapse, "any")
})

test_that("variable blocks take where, visit and concept_col; visit_filter is the alias (F-24)", {
  blk <- function(...) omop_variable_block("blk", "condition_occurrence", c(1, 2), c("a", "b"),
                                           format = "binary", concept_col = "condition_type_concept_id",
                                           where = omop_where(condition_status_concept_id == 4230359), ...)
  b <- blk(visit = list(type = 9201))
  expect_identical(blk(visit_filter = list(concept_ids = 9201)), b)
  expect_error(blk(visit = "index", visit_filter = list(concept_ids = 9201)), "legacy alias of visit")
  expect_error(blk(visit_filter = 9201), "list\\(concept_ids")
  r <- omop_recipe(blocks = b, outputs = omop_output("w", format = "wide"))
  expect_identical(r$variables$a$visit_filter, list(concept_ids = 9201L))
  expect_identical(r$variables$b$concept_col, "condition_type_concept_id")
  expect_s3_class(r$variables$a$where, "omop_predicate")
  plan <- recipe_to_plan(r)
  ev <- plan$events[[plan$variables$a$source$event]]
  expect_identical(ev$visit, list(type = list(9201L)))
  expect_identical(ev$concept_col, "condition_type_concept_id")
  expect_identical(ev$where, list(var = "condition_status_concept_id", op = "==", value = 4230359))
})
