# Phase C: recipe_to_plan must carry per-variable row filters AND index-relative
# time windows into the plan output, in the slots the server actually executes
# (output$filters$custom and output$temporal$index_window). It must also stop
# writing row filters to the dead $filter slot, and round-trip the visit-linkage
# and concept-scope additions on ds.omop.plan.events() / omop_variable().

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- per-variable row filter -> output$filters$custom -------------------------

test_that("recipe_to_plan forwards a per-variable row filter to filters$custom", {
  r <- omop_recipe()
  v <- omop_variable(
    name = "hba1c", table = "measurement", concept_id = 3004410,
    filters = list(omop_filter(
      type = "value_bin", level = "row",
      params = list(var = "value_as_number",
                    value = list(lower = 7, upper = 10))))
  )
  r <- dsOMOPClient:::recipe_add_variable(r, v)
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "labs", type = "long"))

  plan <- recipe_to_plan(r)
  out <- plan$outputs[[1]]

  ft <- out$filters$custom
  expect_false(is.null(ft))
  leaf <- if ("and" %in% names(ft)) ft$and[[1]] else ft
  expect_equal(leaf$op, "value_bin")
  expect_equal(leaf$var, "value_as_number")
  expect_equal(leaf$value$lower, 7)
  expect_equal(leaf$value$upper, 10)
})

test_that("recipe_to_plan no longer writes to the dead output$filter slot", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
    name = "cond", table = "condition_occurrence", concept_id = 201820))
  r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_date_range("2020-01-01", "2023-12-31"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "ev", type = "long"))

  plan <- recipe_to_plan(r)
  out <- plan$outputs$ev

  # Exact-name indexing (not $ partial matching): the executable slot is
  # filters$custom; the legacy filter slot must be entirely absent.
  expect_null(out[["filter"]])
  expect_false(is.null(out$filters$custom))
})

# --- per-variable row filter -> non-long outputs (features/sparse/wide/surv) --
#
# G1 regression: per-variable row filters used to be applied ONLY in the long
# branch; features / wide / covariates_sparse / survival silently dropped them,
# so a value-restricted variable was computed over ALL rows. They must now reach
# the slot the server executes for that output type.

.value_bin_var <- function(name = "hba1c", format = NULL) {
  omop_variable(
    name = name, table = "measurement", concept_id = 3004410, format = format,
    filters = list(omop_filter(
      type = "value_bin", level = "row",
      params = list(var = "value_as_number",
                    value = list(lower = 7, upper = 10)))))
}

.expect_value_bin_leaf <- function(ft) {
  expect_false(is.null(ft))
  leaf <- if ("and" %in% names(ft)) ft$and[[1]] else ft
  expect_equal(leaf$op, "value_bin")
  expect_equal(leaf$var, "value_as_number")
}

test_that("features output forwards per-variable row filter to filters$custom", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, .value_bin_var(format = "mean"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "labs", type = "features"))

  out <- recipe_to_plan(r)$outputs[[1]]
  expect_equal(out$type, "event_level")   # features compiles to event_level
  .expect_value_bin_leaf(out$filters$custom)
})

test_that("covariates_sparse output forwards per-variable row filter", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, .value_bin_var(format = "mean"))
  r <- dsOMOPClient:::recipe_add_output(r,
    omop_output(name = "labs", type = "covariates_sparse"))

  out <- recipe_to_plan(r)$outputs[[1]]
  .expect_value_bin_leaf(out$filters$custom)
})

test_that("survival output forwards per-variable row filter to filters$custom", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, .value_bin_var(format = "mean"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "surv", type = "survival"))

  out <- recipe_to_plan(r)$outputs[["surv"]]
  expect_equal(out$type, "survival")
  .expect_value_bin_leaf(out$filters$custom)
})

test_that("single-table wide features output forwards per-variable row filter", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, .value_bin_var(format = "mean"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "wide", type = "wide"))

  out <- recipe_to_plan(r)$outputs[["wide"]]
  expect_equal(out$type, "event_level")
  .expect_value_bin_leaf(out$filters$custom)
})

test_that("multi-table wide routes row filter into the per-table feature entry", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, .value_bin_var(name = "hba1c", format = "mean"))
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
    name = "sbp", table = "observation", concept_id = 3004249, format = "mean"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "wide", type = "wide"))

  out <- recipe_to_plan(r)$outputs[["wide"]]
  expect_equal(out$type, "person_level")
  # The measurement table entry carries the value_bin; observation does not.
  .expect_value_bin_leaf(out$tables[["measurement"]]$filters)
  expect_null(out$tables[["observation"]]$filters)
})

# --- per-variable time_window -> output$temporal$index_window -----------------

test_that("recipe_to_plan forwards a per-variable time_window as index_window", {
  r <- omop_recipe()
  v <- omop_variable(
    name = "recent_hba1c", table = "measurement", concept_id = 3004410,
    time_window = list(start = -365, end = 0))
  r <- dsOMOPClient:::recipe_add_variable(r, v)
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "labs", type = "long"))

  plan <- recipe_to_plan(r)
  out <- plan$outputs[[1]]

  expect_false(is.null(out$temporal$index_window))
  expect_equal(out$temporal$index_window$start, -365)
  expect_equal(out$temporal$index_window$end, 0)
})

test_that("recipe_to_plan carries row filter AND time_window from one variable", {
  r <- omop_recipe()
  v <- omop_variable(
    name = "hba1c", table = "measurement", concept_id = 3004410,
    time_window = list(start = -90, end = 0),
    filters = list(omop_filter(
      type = "value_bin", level = "row",
      params = list(var = "value_as_number",
                    value = list(lower = 7, upper = 10)))))
  r <- dsOMOPClient:::recipe_add_variable(r, v)
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "labs", type = "long"))

  plan <- recipe_to_plan(r)
  out <- plan$outputs[[1]]

  expect_false(is.null(out$filters$custom))
  expect_equal(out$temporal$index_window$start, -90)
  expect_equal(out$temporal$index_window$end, 0)
})

# --- recipe-level + per-variable row filters merge with AND -------------------

test_that("recipe-level and per-variable row filters AND-merge (neither lost)", {
  r <- omop_recipe()
  v <- omop_variable(
    name = "hba1c", table = "measurement", concept_id = 3004410,
    filters = list(omop_filter(
      type = "value_bin", level = "row",
      params = list(var = "value_as_number",
                    value = list(lower = 7, upper = 10)))))
  r <- dsOMOPClient:::recipe_add_variable(r, v)
  # recipe-level row filter (date range)
  r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_date_range("2020-01-01", "2023-12-31"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "labs", type = "long"))

  plan <- recipe_to_plan(r)
  ft <- plan$outputs[[1]]$filters$custom

  # Top level is an AND with both contributions present.
  expect_true("and" %in% names(ft))
  expect_equal(length(ft$and), 2L)

  # Flatten one level and confirm both the value_bin and the date bounds survive.
  ops <- unlist(lapply(ft$and, function(node) {
    if (!is.null(node$op)) node$op
    else if (!is.null(node$and)) vapply(node$and, function(x) x$op, character(1))
    else NA_character_
  }))
  expect_true("value_bin" %in% ops)
  expect_true(">=" %in% ops)
  expect_true("<=" %in% ops)
})

# --- visit_filter + concept_col round-trip ------------------------------------

test_that("ds.omop.plan.events stores visit_filter and concept_col", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.events(
    plan, name = "ev", table = "measurement", concept_set = 3004410,
    visit_filter = list(concept_ids = 9201L),
    concept_col = "unit_concept_id")

  ev <- plan$outputs$ev
  vf <- ev$filters$visit %||% ev$visit_filter
  cc <- ev$filters$concept_col %||% ev$concept_col
  expect_false(is.null(vf))
  expect_equal(as.integer(unlist(vf$concept_ids %||% vf)), 9201L)
  expect_equal(cc, "unit_concept_id")
})

test_that("recipe_to_plan forwards per-variable visit_filter and concept_col", {
  r <- omop_recipe()
  v <- omop_variable(
    name = "bw", table = "measurement", concept_id = 3025315,
    visit_filter = list(concept_ids = 9201L),
    concept_col = "unit_concept_id")
  r <- dsOMOPClient:::recipe_add_variable(r, v)
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "labs", type = "long"))

  plan <- recipe_to_plan(r)
  out <- plan$outputs[[1]]
  vf <- out$filters$visit %||% out$visit_filter
  cc <- out$filters$concept_col %||% out$concept_col
  expect_false(is.null(vf))
  expect_equal(cc, "unit_concept_id")
})

# --- JSON save/load round-trip ------------------------------------------------

test_that("a recipe round-trips per-variable filter, window, visit, concept_col", {
  r <- omop_recipe()
  v <- omop_variable(
    name = "hba1c", table = "measurement", concept_id = 3004410,
    time_window = list(start = -365, end = 0),
    visit_filter = list(concept_ids = 9201L),
    concept_col = "unit_concept_id",
    filters = list(omop_filter(
      type = "value_bin", level = "row",
      params = list(var = "value_as_number",
                    value = list(lower = 7, upper = 10)))))
  r <- dsOMOPClient:::recipe_add_variable(r, v)
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "labs", type = "long"))

  path <- withr::local_tempfile(fileext = ".json")
  recipe_save(r, path)
  r2 <- recipe_load(path)

  v2 <- r2$variables[["hba1c"]]
  expect_equal(v2$time_window$start, -365)
  expect_equal(v2$time_window$end, 0)
  expect_equal(v2$concept_col, "unit_concept_id")
  expect_false(is.null(v2$visit_filter))
  expect_true(length(v2$filters) >= 1L)

  # And the restored recipe still compiles the filter into the executable slot.
  plan2 <- recipe_to_plan(r2)
  out2 <- plan2$outputs[[1]]
  expect_false(is.null(out2$filters$custom))
  expect_equal(out2$temporal$index_window$start, -365)
})
