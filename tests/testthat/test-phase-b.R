# ==============================================================================
# Phase B client wrappers: ds.omop.cohort.from_table, the unified cohort= arg on
# the exploration wrappers, and prevalence offset/global. Mirrors the existing
# signature + execute=FALSE dry-run code-generation test style (no live server).
# ==============================================================================

# --- 7. ds.omop.cohort.from_table --------------------------------------------

test_that("ds.omop.cohort.from_table is exported with the expected signature", {
  expect_true(is.function(ds.omop.cohort.from_table))
  args <- formals(ds.omop.cohort.from_table)
  expect_true(all(c("x", "new_name", "symbol", "conns") %in% names(args)))
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
})

test_that("ds.omop.cohort.from_table rejects a non-string symbol", {
  expect_error(ds.omop.cohort.from_table(42), "name of a server-side")
})

# --- helpers for the unified cohort= surface ---------------------------------

test_that(".cohort_scope_arg maps each accepted cohort form correctly", {
  h <- structure("dsomop_cohort_fromtbl_1", symbol = ".x",
                 class = "dsomop_cohort_handle")
  expect_equal(.cohort_scope_arg(h), "dsomop_cohort_fromtbl_1")  # handle -> table
  expect_identical(.cohort_scope_arg(5), 5L)                     # id -> integer
  expect_equal(.cohort_scope_arg("dsomop_cohort_def_3"),
               "dsomop_cohort_def_3")                            # table -> as-is
  expect_null(.cohort_scope_arg(NULL))                            # NULL -> NULL
})

# --- 8. unified cohort= on the exploration wrappers --------------------------

test_that("exploration wrappers expose a unified cohort argument", {
  for (fn in list(ds.omop.concept.prevalence, ds.omop.value.counts,
                  ds.omop.column.stats, ds.omop.value.quantiles,
                  ds.omop.value.histogram, ds.omop.missingness,
                  ds.omop.crosstab)) {
    expect_true("cohort" %in% names(formals(fn)))
  }
})

test_that("a cohort handle threads into the generated call as its table name", {
  h <- structure("dsomop_cohort_fromtbl_77", symbol = ".x",
                 class = "dsomop_cohort_handle")

  pv <- ds.omop.concept.prevalence("condition_occurrence", cohort = h,
                                   execute = FALSE)
  expect_true(grepl("dsomop_cohort_fromtbl_77", pv$meta$call_code))

  ct <- ds.omop.crosstab("person", "gender_concept_id", "race_concept_id",
                         cohort = h, execute = FALSE)
  expect_true(grepl("dsomop_cohort_fromtbl_77", ct$meta$call_code))

  cs <- ds.omop.column.stats("measurement", "value_as_number", cohort = h,
                             execute = FALSE)
  expect_true(grepl("dsomop_cohort_fromtbl_77", cs$meta$call_code))
})

test_that("a numeric cohort_definition_id threads through as an id (not a table)", {
  vc <- ds.omop.value.counts("condition_occurrence",
                             "condition_type_concept_id", cohort = 9,
                             execute = FALSE)
  expect_true(grepl("cohort = 9", vc$meta$call_code))
  # It must NOT have been rewritten to a dsomop_cohort_<id> temp-table name.
  expect_false(grepl("dsomop_cohort_9", vc$meta$call_code))
})

# --- 9. prevalence offset + global -------------------------------------------

test_that("ds.omop.concept.prevalence exposes offset + global", {
  args <- formals(ds.omop.concept.prevalence)
  expect_true("offset" %in% names(args))
  expect_true("global" %in% names(args))

  g <- ds.omop.concept.prevalence(global = TRUE, top_n = 20, offset = 500,
                                  execute = FALSE)
  expect_true(grepl("global = TRUE", g$meta$call_code))
  expect_true(grepl("offset = 500", g$meta$call_code))
})
