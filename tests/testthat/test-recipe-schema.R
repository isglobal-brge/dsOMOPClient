# ==============================================================================
# Tests for recipe_preview_schema() enriched output
# ==============================================================================

# Build the B1 recipe used across schema tests.
.b1_recipe <- function() {
  recipe <- omop_recipe()
  recipe <- recipe_add_filter(recipe,
    omop_filter_has_concept(320128, "condition_occurrence"))
  recipe <- recipe_add_variable(recipe, omop_variable_sex(name = "sex"))
  recipe <- recipe_add_variable(recipe,
    omop_variable_age(name = "age", reference = "index"))
  recipe <- recipe_add_variable(recipe,
    omop_variable(name = "n_rhythm", table = "measurement",
                  concept_id = 3022318, concept_name = "Heart rate rhythm",
                  format = "count"))
  recipe_add_output(recipe, omop_output(name = "wide", type = "wide"))
}

test_that("recipe_preview_schema keeps backward-compatible columns and attrs", {
  s <- recipe_preview_schema(.b1_recipe())[["wide"]]
  expect_true(all(c("output", "column", "source", "concept", "type",
                    "format") %in% names(s)))
  expect_equal(attr(s, "join_key"), "person_id")
  expect_equal(attr(s, "output_type"), "wide")
  expect_equal(attr(s, "population_id"), "base")
  expect_true(all(c("person", "measurement") %in% attr(s, "tables")))
})

test_that("recipe_preview_schema pins an implicit person_id row first", {
  s <- recipe_preview_schema(.b1_recipe())[["wide"]]
  expect_equal(s$column[1], "person_id")
  expect_equal(s$source[1], "person.person_id")
  expect_equal(s$r_type[1], "integer")
})

test_that("recipe_preview_schema maps B1 formats to R types with concept names", {
  s <- recipe_preview_schema(.b1_recipe())[["wide"]]
  row <- function(col) s[s$column == col, , drop = FALSE]

  expect_equal(row("sex")$r_type, "factor")
  expect_equal(row("age")$r_type, "numeric")

  nr <- row("n_rhythm")
  expect_equal(nr$r_type, "integer")
  expect_equal(nr$concept_name, "Heart rate rhythm")
  expect_equal(nr$concept, "3022318")
  expect_equal(nr$format, "count")
})

test_that("recipe_preview_schema reports time_window as a string", {
  recipe <- omop_recipe()
  recipe <- recipe_add_variable(recipe,
    omop_variable(name = "sbp", table = "measurement", concept_id = 3004249,
                  concept_name = "Systolic BP", format = "mean",
                  time_window = list(start = -365, end = 0)))
  recipe <- recipe_add_output(recipe, omop_output(name = "w", type = "wide"))
  s <- recipe_preview_schema(recipe)[["w"]]
  expect_equal(s[s$column == "sbp", "time_window"], "[-365,0] d rel index")
  expect_equal(s[s$column == "person_id", "time_window"], "all time")
})

test_that("recipe_preview_schema expands multi-column variables", {
  recipe <- omop_recipe()
  mv <- omop_variable(name = "hba1c", table = "measurement",
                      concept_id = 3004410, concept_name = "HbA1c",
                      format = "mean")
  mv[[".suffix_names"]] <- c("hba1c_1", "hba1c_2", "hba1c_3")
  recipe <- recipe_add_variable(recipe, mv)
  recipe <- recipe_add_output(recipe, omop_output(name = "w", type = "wide"))
  s <- recipe_preview_schema(recipe)[["w"]]
  expect_true(all(c("hba1c_1", "hba1c_2", "hba1c_3") %in% s$column))
  expect_equal(nrow(s[s$concept_name == "HbA1c", ]), 3)
  expect_true(all(s[s$concept_name == "HbA1c", "r_type"] == "numeric"))
})

test_that("recipe_preview_schema marks table_split for multi-table long outputs", {
  recipe <- omop_recipe()
  recipe <- recipe_add_variable(recipe,
    omop_variable(name = "hr", table = "measurement", concept_id = 3022318,
                  concept_name = "Heart rate", format = "mean"))
  recipe <- recipe_add_variable(recipe,
    omop_variable(name = "dx", table = "condition_occurrence",
                  concept_id = 201820, concept_name = "T2DM",
                  format = "binary"))
  recipe <- recipe_add_output(recipe, omop_output(name = "evt", type = "long"))
  s <- recipe_preview_schema(recipe)[["evt"]]
  expect_true("table_split" %in% names(s))
  expect_equal(s[s$column == "person_id", "table_split"], "person")
  expect_equal(s[s$column == "hr", "table_split"], "measurement")
  expect_equal(s[s$column == "dx", "table_split"], "condition_occurrence")
})

test_that("recipe_preview_schema handles empty recipe and variable-less output", {
  expect_equal(recipe_preview_schema(omop_recipe()), list())

  r <- omop_recipe()
  r <- recipe_add_output(r, omop_output(name = "empty", type = "wide"))
  s <- recipe_preview_schema(r)[["empty"]]
  expect_equal(nrow(s), 1)
  expect_equal(s$column, "person_id")
})

test_that("recipe_preview_schema rejects non-recipe input", {
  expect_error(recipe_preview_schema(list()), "omop_recipe")
})
