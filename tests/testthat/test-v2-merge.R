# WS-9 recipe_merge() (design-2.10.3 sections 2.10, 10.1): one namespace, or
# every node of the second recipe renamed with every reference rewritten
# (anchors, censoring, match inputs, output dependencies, publication roots);
# block metadata preserved.

.mrg_template <- function() {
  t2dm <- omop_concept_set(201826, descendants = TRUE, name = "t2dm")
  acei <- omop_era(omop_event("drug_era", 1308216, name = "acei_rows"), name = "acei")
  arb <- omop_era(omop_event("drug_era", 1367500, name = "arb_rows"), name = "arb")
  mi <- omop_event("condition_occurrence", 4329847, name = "mi")
  first_mi <- omop_anchor(mi, which = "first")
  omop_recipe(
    concept_sets = list(t2dm = t2dm),
    events = list(mi, omop_event("condition_occurrence", "t2dm", name = "dm")),
    intervals = list(acei, arb),
    populations = list(
      omop_population("users", index = omop_index(omop_any_of(acei, arb), select = "first_eligible",
                                                  end = omop_end("interval_end")),
                      filters = list(omop_has("dm", window = omop_window(NULL, 0)))),
      omop_population("eligible", parent_id = "users", filters = list(omop_observed(before = 365))),
      omop_population("both", union = c("users", "eligible")),
      omop_population("cc", match = omop_match(cases = "users", controls = "eligible", outcome = "mi",
                                               match = list(omop_strata("dm", before = 365))))),
    variables = list(
      days_to_mi = omop_variable(event = "mi", reducer = omop_reduce("days_until", reference = "index")),
      after_mi = omop_variable(event = "dm", reducer = omop_reduce("count",
                               window = omop_window(omop_bound(first_mi, 0), NULL))),
      arm = omop_population_column("index_source")),
    outputs = list(
      omop_output("base_tab", rows = "users", format = "wide", grain = "episode",
                  columns = c("days_to_mi", "arm")),
      omop_output("tte", rows = "users", format = "survival", options = list(
        outcomes = list(mi = "mi"), tar = omop_window(1, NULL),
        censor = list("observation_end", omop_censor("arb", "first_start_after_entry",
                                                     when_index_source = "acei")))),
      omop_output("joined", rows = "users", format = "joined",
                  options = list(from = list("base_tab", list(output = "base_tab2")))),
      omop_output("base_tab2", rows = "users", format = "wide", grain = "episode",
                  columns = "after_mi")),
    publish = c("tte", "joined"))
}

test_that("a prefixed merge renames every node of the second recipe and rewrites every reference", {
  a <- .mrg_template()
  m <- recipe_merge(a, a, prefix = "b")
  plan <- recipe_to_plan(m)
  expect_setequal(names(plan$populations), c("users", "eligible", "both", "cc",
                                             "b_users", "b_eligible", "b_both", "b_cc"))
  expect_true(all(c("t2dm", "b_t2dm") %in% names(plan$concept_sets)))
  expect_true(all(c("mi", "dm", "b_mi", "b_dm", "acei_rows", "b_acei_rows") %in% names(plan$events)))
  expect_true(all(c("acei", "arb", "b_acei", "b_arb") %in% names(plan$intervals)))
  expect_identical(plan$events$b_dm$concept_set, "b_t2dm")
  bp <- plan$populations
  expect_identical(bp$b_users$index$source$any_of, list(list(interval = "b_acei"), list(interval = "b_arb")))
  expect_identical(bp$b_users$where$has$source, list(event = "b_dm"))
  expect_identical(bp$b_eligible$parent, "b_users")
  expect_identical(unlist(bp$b_both$setop$members), c("b_users", "b_eligible"))
  expect_identical(bp$b_cc$match$cases, "b_users")
  expect_identical(bp$b_cc$match$controls, "b_eligible")
  expect_identical(bp$b_cc$match$outcome, list(event = "b_mi"))
  expect_identical(bp$b_cc$match$match[[1L]]$strata$source, list(event = "b_dm"))
  v <- plan$variables
  expect_identical(v$b_after_mi$reducer$window$start$anchor$source, list(event = "b_mi"))
  expect_identical(v$b_days_to_mi$source, list(event = "b_mi"))
  o <- plan$outputs
  expect_identical(o$b_base_tab$rows, "b_users")
  expect_identical(unlist(o$b_base_tab$columns), c("b_days_to_mi", "b_arm"))
  expect_identical(o$b_base_tab$result_symbol, "D_b_base_tab")
  expect_identical(o$b_tte$options$outcomes$mi, list(event = "b_mi"))
  cz <- o$b_tte$options$censor[[2L]]$censor
  expect_identical(cz$source, list(interval = "b_arb"))
  expect_identical(unlist(cz$when_index_source), "b_acei")
  expect_identical(vapply(o$b_joined$options$from, function(f) f$output, ""),
                   c("b_base_tab", "b_base_tab2"))
  expect_identical(unlist(plan$publish), c("tte", "joined", "b_tte", "b_joined"))
  # the first recipe is untouched
  expect_identical(plan$populations$users, recipe_to_plan(a)$populations$users)
})

test_that("an unprefixed merge shares one namespace and refuses conflicting definitions", {
  a <- omop_recipe(variables = list(age = omop_age()),
                   outputs = omop_output("w1", format = "wide"))
  b <- omop_recipe(variables = list(sex = omop_sex(), age = omop_age()),
                   outputs = omop_output("w2", format = "wide"))
  m <- recipe_merge(a, b)
  plan <- recipe_to_plan(m)
  expect_identical(names(plan$variables), c("age", "sex"))
  # implicit columns were frozen per recipe: w1 keeps age only, w2 its own two
  expect_identical(unlist(plan$outputs$w1$columns), "age")
  expect_identical(unlist(plan$outputs$w2$columns), c("sex", "age"))
  c2 <- omop_recipe(variables = list(age = omop_age(bands = TRUE)),
                    outputs = omop_output("w3", format = "wide"))
  expect_error(recipe_merge(a, c2), "variable 'age'")
  e1 <- omop_recipe(variables = list(n = omop_variable(event = omop_event("measurement", 1, name = "lab"),
                                                       reducer = omop_reduce("count"))),
                    outputs = omop_output("o1", format = "wide"))
  e2 <- omop_recipe(variables = list(k = omop_variable(event = omop_event("measurement", 2, name = "lab"),
                                                       reducer = omop_reduce("count"))),
                    outputs = omop_output("o2", format = "wide"))
  expect_error(recipe_merge(e1, e2), "event 'lab'")
  expect_s3_class(recipe_merge(e1, e2, prefix = "x"), "omop_recipe")
  s1 <- recipe_set_scope(a, cohort = 5)
  expect_error(recipe_merge(s1, b), "scope")
  f1 <- omop_recipe(filters = list(omop_filter_sex("F")), variables = list(age = omop_age()),
                    outputs = omop_output("w1", format = "wide"))
  expect_error(recipe_merge(f1, b), "recipe-level filters")
})

test_that("a prefixed merge keeps the second recipe's base population meaning", {
  a <- omop_recipe(filters = list(omop_filter_sex("F")), variables = list(age = omop_age()),
                   outputs = omop_output("w", format = "wide"))
  b <- omop_recipe(variables = list(age = omop_age()), outputs = omop_output("w", format = "wide"))
  m <- recipe_merge(a, b, prefix = "b")
  plan <- recipe_to_plan(m)
  expect_identical(plan$outputs$b_w$rows, "b_base")
  expect_identical(plan$populations$b_base, list(kind = "criteria"))
  expect_false(is.null(plan$populations$base$where))       # a's sex filter stays on a's base
  # b's recipe-level filters move onto its base
  m2 <- recipe_merge(b, a, prefix = "f")
  p2 <- recipe_to_plan(m2)
  expect_identical(p2$outputs$f_w$rows, "f_base")
  expect_identical(p2$populations$f_base$where, recipe_to_plan(a)$populations$base$where)
  # identical bases are shared
  m3 <- recipe_merge(b, b, prefix = "c")
  expect_identical(recipe_to_plan(m3)$outputs$c_w$rows, "base")
})

test_that("block metadata is preserved and a renamed block re-expands to the prefixed names", {
  blk <- omop_variable_block(id = "dx", table = "condition_occurrence",
                             concept_ids = c(201826, 320128), concept_names = c("t2dm", "htn"),
                             format = "binary")
  a <- omop_recipe(blocks = blk, outputs = omop_output("w", type = "wide"))
  m <- recipe_merge(a, a, prefix = "b")
  expect_identical(names(m$variables), c("t2dm", "htn", "b_t2dm", "b_htn"))
  expect_identical(m$variables$b_t2dm$block_id, "b_dx")
  expect_identical(m$blocks$b_dx$name_prefix, "b")
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(m)), envir = env)
  expect_identical(names(env$recipe$variables), names(m$variables))
  expect_identical(recipe_to_plan(env$recipe, version = 1), recipe_to_plan(m, version = 1))
  back <- recipe_import_json(recipe_export_json(m))
  expect_identical(back$blocks$b_dx$name_prefix, "b")
})

test_that("parameters are shared by name across a merge", {
  lb <- omop_param("lookback", type = "integer", default = 365L)
  mk <- function(cs) omop_recipe(
    variables = list(x = omop_variable(event = omop_event("condition_occurrence", cs),
                                       reducer = omop_reduce("binary", window = omop_window(-lb, 0)))),
    outputs = omop_output("w", format = "wide"))
  m <- recipe_merge(mk(1), mk(2), prefix = "b")
  expect_identical(names(recipe_params(m)), "lookback")
  plan <- recipe_to_plan(recipe_bind(m, lookback = 30L))
  expect_identical(plan$variables$b_x$reducer$window$start, -30L)
  other <- omop_recipe(variables = list(y = omop_variable(event = omop_event("condition_occurrence", 3),
    reducer = omop_reduce("binary", window = omop_window(-omop_param("lookback", type = "integer"), 0)))),
    outputs = omop_output("w2", format = "wide"))
  expect_error(recipe_merge(mk(1), other), "declared with different definitions")
})

test_that("unnamed registry events merge by definition", {
  a <- omop_recipe(events = list(omop_event("death")),
                   variables = list(d = omop_variable(event = omop_event("death"),
                                                      reducer = omop_reduce("binary"))),
                   outputs = omop_output("w", format = "wide"))
  b <- omop_recipe(events = list(omop_event("visit_occurrence", 9201)),
                   variables = list(v = omop_variable(event = omop_event("visit_occurrence", 9201),
                                                      reducer = omop_reduce("count"))),
                   outputs = omop_output("w2", format = "wide"))
  m <- recipe_merge(a, b)
  expect_length(m$events, 2L)
  plan <- recipe_to_plan(m)
  expect_setequal(names(plan$events), c("death", "visit_occurrence_9201"))
})
