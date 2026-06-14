# ==============================================================================
# Tests for recipe_lint() — pure client-side recipe linter
# ==============================================================================

has_code <- function(L, code) code %in% L$code
sev_of   <- function(L, code) L$severity[match(code, L$code)]

# --- ERROR rules --------------------------------------------------------------

test_that("no_compile: output targets a non-base population", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_population(r, omop_population(id = "f", parent_id = "base",
         filters = list(omop_filter_sex("F"))))
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide",
                                        population_id = "f"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "no_compile"))
  expect_equal(sev_of(L, "no_compile"), "ERROR")
})

test_that("empty_output: output resolves to zero variables", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex(name = "sex"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "empty", type = "wide",
                                        variables = "nope"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "empty_output"))
  expect_equal(sev_of(L, "empty_output"), "ERROR")
})

test_that("dup_output: duplicate output names", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "out1", type = "wide"))
  # Force a true duplicate (named-list assignment would otherwise overwrite).
  r$outputs <- c(r$outputs,
    stats::setNames(list(omop_output(name = "out1", type = "wide")), "out1"))
  names(r$outputs) <- c("out1", "out1")
  L <- recipe_lint(r)
  expect_true(has_code(L, "dup_output"))
  expect_equal(sev_of(L, "dup_output"), "ERROR")
})

test_that("concept_zero: unmapped concept on a non-derived format", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(name = "z", table = "measurement",
         concept_id = 0, format = "count"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "concept_zero"))
  expect_equal(sev_of(L, "concept_zero"), "ERROR")
})

test_that("concept_zero: NOT flagged for person-derived formats", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_charlson())
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide"))
  L <- recipe_lint(r)
  expect_false(has_code(L, "concept_zero"))
})

# --- WARNING rules ------------------------------------------------------------

test_that("concept_unnamed: concept_id present but concept_name missing", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(name = "u", table = "measurement",
         concept_id = 3022318, format = "count"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "concept_unnamed"))
  expect_equal(sev_of(L, "concept_unnamed"), "WARNING")
})

test_that("age_no_index: index-age with no cohort and no date filter", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age(reference = "index"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "age_no_index"))
  expect_equal(sev_of(L, "age_no_index"), "WARNING")
})

test_that("age_no_index: suppressed when base has a cohort", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_set_cohort(r, 42)
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age(reference = "index"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide"))
  L <- recipe_lint(r)
  expect_false(has_code(L, "age_no_index"))
})

test_that("narrow_filter: age_range width < 5 years", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_age(min = 40, max = 43))
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "narrow_filter"))
  expect_equal(sev_of(L, "narrow_filter"), "WARNING")
})

test_that("narrow_filter: date_range width < 30 days", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_population(r, omop_population(id = "base",
         label = "All Persons",
         filters = list(omop_filter(type = "date_range", level = "population",
           params = list(start = "2020-01-01", end = "2020-01-10")))))
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "narrow_filter"))
})

test_that("highcard_factor: raw _concept_id column with factor_concepts=TRUE", {
  r <- omop_recipe()  # factor_concepts defaults to TRUE
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(name = "cc",
         table = "condition_occurrence", column = "condition_concept_id",
         format = "raw"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "highcard_factor"))
  expect_equal(sev_of(L, "highcard_factor"), "WARNING")
})

test_that("window_inverted: time_window start > end", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(name = "w", table = "measurement",
         concept_id = 3022318, concept_name = "HR", format = "count",
         time_window = list(start = 30, end = -30)))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "out", type = "wide"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "window_inverted"))
  expect_equal(sev_of(L, "window_inverted"), "WARNING")
})

# --- INFO rules ---------------------------------------------------------------

test_that("long_split: long output spanning >1 table", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(name = "a",
         table = "condition_occurrence", concept_id = 1, concept_name = "A",
         column = "x", format = "raw"))
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(name = "b", table = "measurement",
         concept_id = 2, concept_name = "B", column = "y", format = "raw"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "evt", type = "long"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "long_split"))
  expect_equal(sev_of(L, "long_split"), "INFO")
})

test_that("no_value_source: aggregate format without a value_source", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(name = "m", table = "measurement",
         concept_id = 3004249, concept_name = "SBP", format = "mean"))
  r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w", type = "wide"))
  L <- recipe_lint(r)
  expect_true(has_code(L, "no_value_source"))
  expect_equal(sev_of(L, "no_value_source"), "INFO")
})

# --- Edge cases & contract ----------------------------------------------------

test_that("edge: NULL recipe yields a single empty_recipe info row", {
  L <- recipe_lint(NULL)
  expect_equal(nrow(L), 1L)
  expect_equal(L$code, "empty_recipe")
  expect_equal(L$severity, "INFO")
})

test_that("edge: recipe with zero outputs yields a no_output warning", {
  r <- omop_recipe()
  r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
  L <- recipe_lint(r)
  expect_true(has_code(L, "no_output"))
  expect_equal(sev_of(L, "no_output"), "WARNING")
})

test_that("recipe_lint returns the 4-column data.frame contract", {
  L <- recipe_lint(omop_recipe())
  expect_s3_class(L, "data.frame")
  expect_named(L, c("severity", "code", "message", "locus"))
})
