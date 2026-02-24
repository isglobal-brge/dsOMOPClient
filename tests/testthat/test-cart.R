# ==============================================================================
# Tests for cart / recipe infrastructure
# ==============================================================================

# --- omop_variable -----------------------------------------------------------

test_that("omop_variable creates correct class", {
  v <- omop_variable(
    name = "diabetes_flag",
    table = "condition_occurrence",
    concept_id = 201820,
    concept_name = "Type 2 diabetes mellitus",
    format = "binary"
  )
  expect_s3_class(v, "omop_variable")
  expect_equal(v$name, "diabetes_flag")
  expect_equal(v$table, "condition_occurrence")
  expect_equal(v$concept_id, 201820L)
  expect_equal(v$concept_name, "Type 2 diabetes mellitus")
  expect_equal(v$format, "binary")
  expect_equal(v$type, "auto")
})

test_that("omop_variable auto-generates name from concept_name", {
  v <- omop_variable(
    table = "condition_occurrence",
    concept_name = "Type 2 Diabetes Mellitus"
  )
  expect_equal(v$name, "type_2_diabetes_mellitus")
})

test_that("omop_variable auto-generates name from concept_id", {
  v <- omop_variable(table = "measurement", concept_id = 3004249)
  expect_equal(v$name, "measurement_c3004249")
})

test_that("omop_variable auto-generates name from column", {
  v <- omop_variable(table = "person", column = "year_of_birth")
  expect_equal(v$name, "person_year_of_birth")
})

test_that("omop_variable print works", {
  v <- omop_variable(
    name = "test_var",
    table = "measurement",
    concept_id = 3004249,
    concept_name = "Hemoglobin",
    format = "mean"
  )
  out <- capture.output(print(v))
  expect_true(any(grepl("omop_variable", out)))
  expect_true(any(grepl("measurement", out)))
  expect_true(any(grepl("3004249", out)))
})

# --- omop_filter -------------------------------------------------------------

test_that("omop_filter creates correct class", {
  f <- omop_filter(type = "sex", level = "population",
                    params = list(value = "F"))
  expect_s3_class(f, "omop_filter")
  expect_equal(f$type, "sex")
  expect_equal(f$level, "population")
  expect_equal(f$params$value, "F")
  expect_true(grepl("Sex", f$label))
})

test_that("omop_filter_sex convenience constructor", {
  f <- omop_filter_sex("M")
  expect_s3_class(f, "omop_filter")
  expect_equal(f$type, "sex")
  expect_equal(f$level, "population")
  expect_equal(f$params$value, "M")
})

test_that("omop_filter_age convenience constructor", {
  f <- omop_filter_age(min = 18, max = 65)
  expect_s3_class(f, "omop_filter")
  expect_equal(f$type, "age_range")
  expect_equal(f$params$min, 18)
  expect_equal(f$params$max, 65)
})

test_that("omop_filter_has_concept convenience constructor", {
  f <- omop_filter_has_concept(201820, "condition_occurrence", "Diabetes")
  expect_s3_class(f, "omop_filter")
  expect_equal(f$type, "has_concept")
  expect_equal(f$level, "population")
  expect_equal(f$params$concept_id, 201820L)
  expect_true(grepl("Diabetes", f$label))
})

test_that("omop_filter_date_range constructor", {
  f <- omop_filter_date_range(start = "2020-01-01", end = "2023-12-31")
  expect_s3_class(f, "omop_filter")
  expect_equal(f$type, "date_range")
  expect_equal(f$level, "row")
  expect_equal(f$params$start, "2020-01-01")
})

test_that("omop_filter_value constructor", {
  f <- omop_filter_value(op = ">=", value = 6.5)
  expect_s3_class(f, "omop_filter")
  expect_equal(f$type, "value_threshold")
  expect_equal(f$level, "row")
  expect_equal(f$params$op, ">=")
  expect_equal(f$params$value, 6.5)
})

test_that("omop_filter print works", {
  f <- omop_filter_sex("F")
  out <- capture.output(print(f))
  expect_true(any(grepl("omop_filter", out)))
  expect_true(any(grepl("population", out)))
})

# --- omop_output -------------------------------------------------------------

test_that("omop_output creates correct class", {
  o <- omop_output(name = "patient_data", type = "wide")
  expect_s3_class(o, "omop_output")
  expect_equal(o$name, "patient_data")
  expect_equal(o$type, "wide")
  expect_null(o$variables)
})

test_that("omop_output with variables subset", {
  o <- omop_output(name = "events", type = "long",
                    variables = c("diabetes_flag", "hemoglobin"))
  expect_equal(o$variables, c("diabetes_flag", "hemoglobin"))
})

test_that("omop_output print works", {
  o <- omop_output(name = "test_out", type = "features",
                    variables = c("a", "b"))
  out <- capture.output(print(o))
  expect_true(any(grepl("omop_output", out)))
  expect_true(any(grepl("features", out)))
})

# --- omop_cart ---------------------------------------------------------------

test_that("omop_cart creates empty cart", {
  c <- omop_cart()
  expect_s3_class(c, "omop_cart")
  expect_equal(length(c$variables), 0)
  expect_equal(length(c$filters), 0)
  expect_equal(length(c$outputs), 0)
  expect_true(inherits(c$meta$created, "POSIXct"))
})

test_that("cart_add_variable adds variable", {
  c <- omop_cart()
  v <- omop_variable(name = "test", table = "person",
                      column = "year_of_birth")
  c <- cart_add_variable(c, v)
  expect_equal(length(c$variables), 1)
  expect_equal(c$variables$test$table, "person")
})

test_that("cart_add_variable with inline args", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "test", table = "person",
                          column = "year_of_birth")
  expect_equal(length(c$variables), 1)
  expect_equal(c$variables$test$column, "year_of_birth")
})

test_that("cart_add_variable rejects non-cart", {
  expect_error(cart_add_variable(list(), omop_variable(table = "person")),
               "omop_cart")
})

test_that("cart_remove_variable removes by name", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "a", table = "person")
  c <- cart_add_variable(c, name = "b", table = "person")
  expect_equal(length(c$variables), 2)
  c <- cart_remove_variable(c, "a")
  expect_equal(length(c$variables), 1)
  expect_null(c$variables$a)
  expect_true(!is.null(c$variables$b))
})

test_that("cart_add_filter adds filter", {
  c <- omop_cart()
  f <- omop_filter_sex("F")
  c <- cart_add_filter(c, f)
  expect_equal(length(c$filters), 1)
})

test_that("cart_add_filter auto-generates ID", {
  c <- omop_cart()
  c <- cart_add_filter(c, omop_filter_sex("F"))
  id <- names(c$filters)[1]
  expect_true(grepl("sex", id))
})

test_that("cart_add_filter with explicit ID", {
  c <- omop_cart()
  c <- cart_add_filter(c, omop_filter_sex("F"), id = "my_filter")
  expect_true("my_filter" %in% names(c$filters))
})

test_that("cart_remove_filter removes by ID", {
  c <- omop_cart()
  c <- cart_add_filter(c, omop_filter_sex("F"), id = "f1")
  c <- cart_add_filter(c, omop_filter_age(18, 65), id = "f2")
  c <- cart_remove_filter(c, "f1")
  expect_equal(length(c$filters), 1)
  expect_null(c$filters$f1)
})

test_that("cart_add_output adds output", {
  c <- omop_cart()
  o <- omop_output(name = "wide_out", type = "wide")
  c <- cart_add_output(c, o)
  expect_equal(length(c$outputs), 1)
  expect_equal(c$outputs$wide_out$type, "wide")
})

test_that("cart_remove_output removes by name", {
  c <- omop_cart()
  c <- cart_add_output(c, omop_output(name = "a", type = "wide"))
  c <- cart_add_output(c, omop_output(name = "b", type = "long"))
  c <- cart_remove_output(c, "a")
  expect_equal(length(c$outputs), 1)
  expect_null(c$outputs$a)
})

test_that("cart_clear returns empty cart", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "x", table = "person")
  c <- cart_add_filter(c, omop_filter_sex("M"))
  c <- cart_add_output(c, omop_output(name = "o", type = "wide"))
  c <- cart_clear(c)
  expect_s3_class(c, "omop_cart")
  expect_equal(length(c$variables), 0)
  expect_equal(length(c$filters), 0)
  expect_equal(length(c$outputs), 0)
})

test_that("cart print works", {
  c <- omop_cart()
  c <- cart_add_variable(c,
    name = "diab", table = "condition_occurrence",
    concept_id = 201820, concept_name = "Diabetes",
    format = "binary")
  c <- cart_add_filter(c, omop_filter_sex("F"), id = "sex_f")
  c <- cart_add_output(c, omop_output(name = "wide_out", type = "wide"))
  out <- capture.output(print(c))
  expect_true(any(grepl("omop_cart", out)))
  expect_true(any(grepl("diab", out)))
  expect_true(any(grepl("population", out)))
  expect_true(any(grepl("wide", out)))
})

# --- cart_to_plan -----------------------------------------------------------

test_that("cart_to_plan generates plan with wide output", {
  c <- omop_cart()
  c <- cart_add_variable(c,
    name = "yob", table = "person", column = "year_of_birth")
  c <- cart_add_output(c, omop_output(name = "demographics", type = "wide"))
  plan <- cart_to_plan(c)
  expect_s3_class(plan, "omop_plan")
  expect_true("demographics" %in% names(plan$outputs))
  expect_equal(plan$outputs$demographics$type, "person_level")
})

test_that("cart_to_plan generates plan with long output", {
  c <- omop_cart()
  c <- cart_add_variable(c,
    name = "cond", table = "condition_occurrence",
    concept_id = 201820, format = "raw")
  c <- cart_add_output(c, omop_output(name = "events", type = "long"))
  plan <- cart_to_plan(c)
  expect_true("events" %in% names(plan$outputs))
})

test_that("cart_to_plan applies population filters", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "yob", table = "person",
                          column = "year_of_birth")
  c <- cart_add_filter(c, omop_filter_sex("F"), id = "sex_filter")
  c <- cart_add_output(c, omop_output(name = "out", type = "wide"))
  plan <- cart_to_plan(c)
  expect_true(!is.null(plan$cohort))
  expect_equal(plan$cohort$type, "spec")
})

test_that("cart_to_plan rejects non-cart", {
  expect_error(cart_to_plan(list()), "omop_cart")
})

# --- cart_to_code -----------------------------------------------------------

test_that("cart_to_code generates R code", {
  c <- omop_cart()
  c <- cart_add_variable(c,
    name = "hba1c", table = "measurement",
    concept_id = 3004249, format = "mean")
  c <- cart_add_filter(c, omop_filter_sex("F"))
  c <- cart_add_output(c, omop_output(name = "data", type = "wide"))
  code <- cart_to_code(c)
  expect_true(nchar(code) > 0)
  expect_true(grepl("omop_variable", code))
  expect_true(grepl("omop_filter_sex", code))
  expect_true(grepl("omop_output", code))
})

test_that("cart_to_code rejects non-cart", {
  expect_error(cart_to_code(list()), "omop_cart")
})

# --- cart_preview_schema ----------------------------------------------------

test_that("cart_preview_schema returns schema per output", {
  c <- omop_cart()
  c <- cart_add_variable(c,
    name = "yob", table = "person", column = "year_of_birth",
    type = "numeric")
  c <- cart_add_variable(c,
    name = "diab", table = "condition_occurrence",
    concept_id = 201820, format = "binary")
  c <- cart_add_output(c, omop_output(name = "out1", type = "wide"))
  schemas <- cart_preview_schema(c)
  expect_true(is.list(schemas))
  expect_true("out1" %in% names(schemas))
  df <- schemas$out1
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 2)
  expect_true("yob" %in% df$column)
  expect_true("diab" %in% df$column)
  expect_equal(attr(df, "join_key"), "person_id")
})

test_that("cart_preview_schema with subset variables", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "a", table = "person", column = "x")
  c <- cart_add_variable(c, name = "b", table = "person", column = "y")
  c <- cart_add_output(c, omop_output(name = "out", type = "wide",
                                       variables = c("a")))
  schemas <- cart_preview_schema(c)
  expect_equal(nrow(schemas$out), 1)
  expect_equal(schemas$out$column, "a")
})

test_that("cart_preview_schema empty cart", {
  c <- omop_cart()
  c <- cart_add_output(c, omop_output(name = "empty_out", type = "wide"))
  schemas <- cart_preview_schema(c)
  expect_equal(nrow(schemas$empty_out), 0)
})

# --- .sanitize_name ----------------------------------------------------------

test_that(".sanitize_name handles various inputs", {
  expect_equal(.sanitize_name("Type 2 Diabetes Mellitus"),
               "type_2_diabetes_mellitus")
  expect_equal(.sanitize_name("HbA1c (%)"), "hba1c")
  expect_equal(.sanitize_name("123_bad"), "v_123_bad")
  expect_equal(.sanitize_name("  spaces  "), "spaces")
})

test_that(".sanitize_name handles empty string", {
  expect_equal(.sanitize_name(""), "var")
})

test_that(".sanitize_name handles special characters", {
  expect_equal(.sanitize_name("a + b / c"), "a_b_c")
  expect_equal(.sanitize_name("---test---"), "test")
  expect_equal(.sanitize_name("CamelCase"), "camelcase")
})

test_that(".sanitize_name truncates long names", {
  long <- paste(rep("a", 60), collapse = "")
  result <- .sanitize_name(long)
  expect_true(nchar(result) <= 50)
})

# --- .ensure_unique_name -----------------------------------------------------

test_that(".ensure_unique_name returns name when no conflict", {
  expect_equal(.ensure_unique_name("foo", c("bar", "baz")), "foo")
})

test_that(".ensure_unique_name adds _2 suffix on first collision", {
  expect_equal(.ensure_unique_name("foo", c("foo", "bar")), "foo_2")
})

test_that(".ensure_unique_name increments suffix", {
  expect_equal(.ensure_unique_name("foo", c("foo", "foo_2", "foo_3")), "foo_4")
})

test_that(".ensure_unique_name handles empty existing", {
  expect_equal(.ensure_unique_name("test", character(0)), "test")
})

test_that(".ensure_unique_name escapes R reserved words", {
  expect_equal(.ensure_unique_name("if", character(0)), "if_var")
  expect_equal(.ensure_unique_name("TRUE", character(0)), "TRUE_var")
  expect_equal(.ensure_unique_name("NULL", character(0)), "NULL_var")
  expect_equal(.ensure_unique_name("for", character(0)), "for_var")
})

test_that(".ensure_unique_name escapes reserved then deduplicates", {
  expect_equal(.ensure_unique_name("if", c("if_var")), "if_var_2")
})

# --- .suffix_names -----------------------------------------------------------

test_that(".suffix_names index mode", {
  result <- .suffix_names("hba1c", 3, mode = "index")
  expect_equal(result, c("hba1c_1", "hba1c_2", "hba1c_3"))
})

test_that(".suffix_names returns base_name for n=1", {
  expect_equal(.suffix_names("foo", 1, mode = "index"), "foo")
})

test_that(".suffix_names range mode", {
  ranges <- matrix(c(0, 30, 31, 90, 91, 365), ncol = 2, byrow = TRUE)
  result <- .suffix_names("drug", 3, mode = "range", ranges = ranges)
  expect_equal(result, c("drug_d0_30", "drug_d31_90", "drug_d91_365"))
})

test_that(".suffix_names label mode", {
  result <- .suffix_names("med", 2, mode = "label",
                           labels = c("Aspirin", "Metformin"))
  expect_equal(result, c("med_aspirin", "med_metformin"))
})

test_that(".suffix_names falls back to index when labels missing", {
  result <- .suffix_names("med", 3, mode = "label", labels = c("a"))
  expect_equal(result, c("med_1", "med_2", "med_3"))
})

test_that(".suffix_names falls back to index when ranges missing", {
  result <- .suffix_names("x", 2, mode = "range")
  expect_equal(result, c("x_1", "x_2"))
})

# --- omop_filter_group -------------------------------------------------------

test_that("omop_filter_group creates AND group", {
  f1 <- omop_filter_sex("F")
  f2 <- omop_filter_age(min = 18, max = 65)
  g <- omop_filter_group(f1, f2, operator = "AND")
  expect_s3_class(g, "omop_filter_group")
  expect_equal(g$operator, "AND")
  expect_equal(length(g$children), 2)
  expect_true(grepl("AND", g$label))
})

test_that("omop_filter_group creates OR group", {
  f1 <- omop_filter_sex("F")
  f2 <- omop_filter_sex("M")
  g <- omop_filter_group(f1, f2, operator = "OR")
  expect_equal(g$operator, "OR")
  expect_true(grepl("OR", g$label))
})

test_that("omop_filter_group allows nested groups", {
  f1 <- omop_filter_sex("F")
  f2 <- omop_filter_age(18, 65)
  inner <- omop_filter_group(f1, f2, operator = "AND")
  f3 <- omop_filter_has_concept(201820, "condition_occurrence")
  outer <- omop_filter_group(inner, f3, operator = "OR")
  expect_s3_class(outer, "omop_filter_group")
  expect_s3_class(outer$children[[1]], "omop_filter_group")
  expect_s3_class(outer$children[[2]], "omop_filter")
})

test_that("omop_filter_group rejects non-filter arguments", {
  expect_error(
    omop_filter_group("not_a_filter", operator = "AND"),
    "omop_filter"
  )
})

test_that("omop_filter_group custom label", {
  f1 <- omop_filter_sex("F")
  f2 <- omop_filter_age(18, 65)
  g <- omop_filter_group(f1, f2, operator = "AND",
                          label = "Adult females")
  expect_equal(g$label, "Adult females")
})

test_that("omop_filter_group print works", {
  f1 <- omop_filter_sex("F")
  f2 <- omop_filter_age(18, 65)
  g <- omop_filter_group(f1, f2, operator = "AND")
  out <- capture.output(print(g))
  expect_true(any(grepl("omop_filter_group", out)))
  expect_true(any(grepl("AND", out)))
})

# --- omop_population ---------------------------------------------------------

test_that("omop_population creates correct class", {
  p <- omop_population(id = "adults", label = "Adults 18+",
                        parent_id = "base")
  expect_s3_class(p, "omop_population")
  expect_equal(p$id, "adults")
  expect_equal(p$label, "Adults 18+")
  expect_equal(p$parent_id, "base")
  expect_null(p$cohort_definition_id)
})

test_that("omop_population with cohort ID", {
  p <- omop_population(id = "cohort_pop", cohort_definition_id = 42)
  expect_equal(p$cohort_definition_id, 42L)
})

test_that("omop_population with filters", {
  f <- omop_filter_age(18, 65)
  p <- omop_population(id = "working_age", parent_id = "base",
                        filters = list(f))
  expect_equal(length(p$filters), 1)
})

test_that("omop_population print works", {
  p <- omop_population(id = "test_pop", label = "Test", parent_id = "base")
  out <- capture.output(print(p))
  expect_true(any(grepl("omop_population", out)))
  expect_true(any(grepl("base", out)))
})

# --- omop_variable_block -----------------------------------------------------

test_that("omop_variable_block creates correct class", {
  b <- omop_variable_block(
    id = "cond_block", table = "condition_occurrence",
    concept_ids = c(201820, 4229440),
    concept_names = c("Diabetes", "Hypertension"),
    format = "binary"
  )
  expect_s3_class(b, "omop_variable_block")
  expect_equal(b$id, "cond_block")
  expect_equal(b$table, "condition_occurrence")
  expect_equal(b$concept_ids, c(201820L, 4229440L))
  expect_equal(b$format, "binary")
  expect_equal(b$population_id, "base")
})

test_that("omop_variable_block auto-generates id", {
  b <- omop_variable_block(table = "measurement",
                            concept_ids = c(1, 2, 3))
  expect_true(grepl("block_measurement_3", b$id))
})

test_that("omop_variable_block with time window", {
  b <- omop_variable_block(
    table = "drug_exposure",
    concept_ids = c(1, 2),
    time_window = list(start = -365, end = 0)
  )
  expect_equal(b$time_window$start, -365)
  expect_equal(b$time_window$end, 0)
})

test_that("omop_variable_block print works", {
  b <- omop_variable_block(
    table = "condition_occurrence",
    concept_ids = c(1, 2, 3), format = "binary"
  )
  out <- capture.output(print(b))
  expect_true(any(grepl("omop_variable_block", out)))
  expect_true(any(grepl("condition_occurrence", out)))
  expect_true(any(grepl("3", out)))
})

# --- omop_output enhanced types ----------------------------------------------

test_that("omop_output supports baseline type", {
  o <- omop_output(name = "demo", type = "baseline")
  expect_equal(o$type, "baseline")
})

test_that("omop_output supports joined_long type", {
  o <- omop_output(name = "all_events", type = "joined_long")
  expect_equal(o$type, "joined_long")
})

test_that("omop_output supports covariates_sparse type", {
  o <- omop_output(name = "features", type = "covariates_sparse")
  expect_equal(o$type, "covariates_sparse")
})

test_that("omop_output with population_id", {
  o <- omop_output(name = "adult_data", type = "wide",
                    population_id = "adults")
  expect_equal(o$population_id, "adults")
})

# --- omop_variable enhanced fields -------------------------------------------

test_that("omop_variable with time_window", {
  v <- omop_variable(name = "recent_drug", table = "drug_exposure",
                      concept_id = 123,
                      time_window = list(start = -90, end = 0))
  expect_equal(v$time_window$start, -90)
  expect_equal(v$time_window$end, 0)
})

test_that("omop_variable with value_source", {
  v <- omop_variable(name = "hba1c", table = "measurement",
                      concept_id = 3004249,
                      value_source = "value_as_number",
                      format = "mean")
  expect_equal(v$value_source, "value_as_number")
})

test_that("omop_variable with all format options", {
  for (fmt in c("raw", "binary", "count", "first_value", "last_value",
                "mean", "min", "max", "time_since", "binned")) {
    v <- omop_variable(name = paste0("v_", fmt), table = "t", format = fmt)
    expect_equal(v$format, fmt)
  }
})

test_that("omop_variable with all type options", {
  for (tp in c("auto", "numeric", "categorical", "date", "boolean",
               "integer", "character")) {
    v <- omop_variable(name = paste0("v_", tp), table = "t", type = tp)
    expect_equal(v$type, tp)
  }
})

test_that("omop_variable with row-level filters", {
  f <- omop_filter_value(op = ">=", value = 6.5)
  v <- omop_variable(name = "hba1c_high", table = "measurement",
                      concept_id = 3004249, filters = list(f))
  expect_equal(length(v$filters), 1)
})

# --- omop_filter enhanced types ----------------------------------------------

test_that("omop_filter supports all types", {
  for (tp in c("sex", "age_range", "cohort", "has_concept", "date_range",
               "value_threshold", "concept_set", "min_count", "top_n",
               "dedup", "custom")) {
    f <- omop_filter(type = tp, level = "population",
                      params = list(value = "test"))
    expect_equal(f$type, tp)
  }
})

test_that("omop_filter auto-labels concept_set type", {
  f <- omop_filter(type = "concept_set", level = "row",
                    params = list(concept_ids = c(1, 2, 3)))
  expect_true(grepl("3", f$label))
})

test_that("omop_filter auto-labels min_count type", {
  f <- omop_filter(type = "min_count", level = "row",
                    params = list(min_count = 5))
  expect_true(grepl("5", f$label))
})

test_that("omop_filter_has_concept with min_count", {
  f <- omop_filter_has_concept(201820, "condition_occurrence",
                                min_count = 3L)
  expect_equal(f$params$min_count, 3L)
  expect_true(grepl(">=3", f$label))
})

test_that("omop_filter_has_concept with window", {
  f <- omop_filter_has_concept(201820, "condition_occurrence",
                                window = list(start = -365, end = 0))
  expect_equal(f$params$window$start, -365)
})

test_that("omop_filter_value with custom column", {
  f <- omop_filter_value(op = ">", value = 100,
                          column = "quantity")
  expect_equal(f$params$column, "quantity")
})

# --- cart populations --------------------------------------------------------

test_that("omop_cart has default base population", {
  c <- omop_cart()
  expect_true("base" %in% names(c$populations))
  expect_s3_class(c$populations$base, "omop_population")
  expect_equal(c$populations$base$label, "All Persons")
})

test_that("cart_add_population adds population", {
  c <- omop_cart()
  p <- omop_population(id = "adults", label = "Adults", parent_id = "base")
  c <- cart_add_population(c, p)
  expect_true("adults" %in% names(c$populations))
  expect_equal(c$populations$adults$label, "Adults")
})

test_that("cart_add_population validates parent exists", {
  c <- omop_cart()
  p <- omop_population(id = "sub", label = "Sub", parent_id = "nonexistent")
  expect_error(cart_add_population(c, p), "not found")
})

test_that("cart_remove_population works", {
  c <- omop_cart()
  p <- omop_population(id = "test", label = "Test", parent_id = "base")
  c <- cart_add_population(c, p)
  c <- cart_remove_population(c, "test")
  expect_null(c$populations$test)
})

test_that("cart_remove_population cannot remove base", {
  c <- omop_cart()
  expect_error(cart_remove_population(c, "base"), "Cannot remove base")
})

# --- cart blocks -------------------------------------------------------------

test_that("cart_add_block expands concepts into variables", {
  c <- omop_cart()
  b <- omop_variable_block(
    id = "cond_block", table = "condition_occurrence",
    concept_ids = c(201820, 4229440),
    concept_names = c("Diabetes", "Hypertension"),
    format = "binary"
  )
  c <- cart_add_block(c, b)
  expect_true("cond_block" %in% names(c$blocks))
  expect_equal(length(c$variables), 2)
  # Check naming
  expect_true("diabetes" %in% names(c$variables))
  expect_true("hypertension" %in% names(c$variables))
  # Check format propagation
  expect_equal(c$variables$diabetes$format, "binary")
  expect_equal(c$variables$hypertension$format, "binary")
})

test_that("cart_add_block uses concept_id for naming when no name", {
  c <- omop_cart()
  b <- omop_variable_block(
    table = "measurement",
    concept_ids = c(3004249, 3013290)
  )
  c <- cart_add_block(c, b)
  expect_true("measurement_c3004249" %in% names(c$variables))
  expect_true("measurement_c3013290" %in% names(c$variables))
})

test_that("cart_add_block deduplicates variable names", {
  c <- omop_cart()
  # Add a variable with same name that block would generate
  c <- cart_add_variable(c, name = "diabetes", table = "person")

  b <- omop_variable_block(
    table = "condition_occurrence",
    concept_ids = c(201820),
    concept_names = c("Diabetes"),
    format = "binary"
  )
  c <- cart_add_block(c, b)
  expect_true("diabetes_2" %in% names(c$variables))
})

test_that("cart_add_block propagates time_window", {
  c <- omop_cart()
  b <- omop_variable_block(
    table = "drug_exposure",
    concept_ids = c(1, 2),
    time_window = list(start = -90, end = 0),
    format = "count"
  )
  c <- cart_add_block(c, b)
  for (v in c$variables) {
    expect_equal(v$time_window$start, -90)
    expect_equal(v$time_window$end, 0)
  }
})

test_that("cart_add_block propagates filters", {
  c <- omop_cart()
  f <- omop_filter_date_range(start = "2020-01-01", end = "2023-12-31")
  b <- omop_variable_block(
    table = "measurement",
    concept_ids = c(100),
    format = "mean",
    filters = list(f)
  )
  c <- cart_add_block(c, b)
  v <- c$variables[[1]]
  expect_equal(length(v$filters), 1)
})

# --- cart_add_variable unique naming -----------------------------------------

test_that("cart_add_variable auto-deduplicates names", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "test", table = "person")
  c <- cart_add_variable(c, name = "test", table = "person")
  expect_equal(length(c$variables), 2)
  expect_true("test" %in% names(c$variables))
  expect_true("test_2" %in% names(c$variables))
})

test_that("cart_add_variable continues incrementing", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "x", table = "person")
  c <- cart_add_variable(c, name = "x", table = "person")
  c <- cart_add_variable(c, name = "x", table = "person")
  expect_equal(length(c$variables), 3)
  expect_equal(names(c$variables), c("x", "x_2", "x_3"))
})

# --- cart_add_filter with groups ---------------------------------------------

test_that("cart_add_filter accepts filter group", {
  c <- omop_cart()
  g <- omop_filter_group(
    omop_filter_sex("F"),
    omop_filter_age(18, 65),
    operator = "AND"
  )
  c <- cart_add_filter(c, g)
  expect_equal(length(c$filters), 1)
  expect_s3_class(c$filters[[1]], "omop_filter_group")
})

test_that("cart_add_filter group auto-generates ID with operator", {
  c <- omop_cart()
  g <- omop_filter_group(
    omop_filter_sex("F"),
    omop_filter_age(18, 65),
    operator = "OR"
  )
  c <- cart_add_filter(c, g)
  id <- names(c$filters)[1]
  expect_true(grepl("OR", id))
})

# --- .flatten_filters --------------------------------------------------------

test_that(".flatten_filters extracts population level", {
  filters <- list(
    omop_filter_sex("F"),
    omop_filter_date_range("2020-01-01", "2023-12-31"),
    omop_filter_age(18, 65)
  )
  pop <- .flatten_filters(filters, level = "population")
  expect_equal(length(pop), 2)  # sex + age
  row <- .flatten_filters(filters, level = "row")
  expect_equal(length(row), 1)  # date_range
})

test_that(".flatten_filters handles nested groups", {
  g <- omop_filter_group(
    omop_filter_sex("F"),
    omop_filter_group(
      omop_filter_age(18, 65),
      omop_filter_has_concept(201820, "condition_occurrence"),
      operator = "AND"
    ),
    operator = "OR"
  )
  pop <- .flatten_filters(list(g), level = "population")
  expect_equal(length(pop), 3)  # sex, age, has_concept
})

test_that(".flatten_filters with NULL level returns all", {
  filters <- list(
    omop_filter_sex("F"),
    omop_filter_date_range("2020-01-01", "2023-12-31")
  )
  all_filters <- .flatten_filters(filters, level = NULL)
  expect_equal(length(all_filters), 2)
})

# --- cart_to_plan with populations and blocks --------------------------------

test_that("cart_to_plan with cohort population", {
  c <- omop_cart()
  c$populations$base$cohort_definition_id <- 42L
  c <- cart_add_variable(c, name = "v", table = "person",
                          column = "year_of_birth")
  c <- cart_add_output(c, omop_output(name = "out", type = "wide"))
  plan <- cart_to_plan(c)
  expect_true(!is.null(plan$cohort))
  expect_equal(plan$cohort$cohort_definition_id, 42L)
})

test_that("cart_to_plan with features output", {
  c <- omop_cart()
  c <- cart_add_variable(c,
    name = "diabetes", table = "condition_occurrence",
    concept_id = 201820, format = "binary")
  c <- cart_add_variable(c,
    name = "aspirin_count", table = "drug_exposure",
    concept_id = 1154070, format = "count")
  c <- cart_add_output(c, omop_output(name = "feat", type = "features"))
  plan <- cart_to_plan(c)
  # Features from two tables -> two output entries
  feat_names <- grep("feat", names(plan$outputs), value = TRUE)
  expect_true(length(feat_names) >= 1)
})

test_that("cart_to_plan with row-level date filter", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "v", table = "condition_occurrence",
                          concept_id = 201820)
  c <- cart_add_filter(c, omop_filter_date_range("2020-01-01", "2023-12-31"))
  c <- cart_add_output(c, omop_output(name = "events", type = "long"))
  plan <- cart_to_plan(c)
  # Row filter should be attached to the output
  out <- plan$outputs$events
  expect_true(!is.null(out$filters))
  expect_true(!is.null(out$filters$time_window))
})

test_that("cart_to_plan with baseline output type", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "yob", table = "person",
                          column = "year_of_birth")
  c <- cart_add_output(c, omop_output(name = "demo", type = "baseline"))
  plan <- cart_to_plan(c)
  expect_true("demo" %in% names(plan$outputs))
  expect_equal(plan$outputs$demo$type, "baseline")
})

test_that("cart_to_plan with survival output type", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "event", table = "condition_occurrence",
                          concept_id = 201820)
  c <- cart_add_output(c, omop_output(name = "surv", type = "survival"))
  plan <- cart_to_plan(c)
  expect_true("surv" %in% names(plan$outputs))
  expect_equal(plan$outputs$surv$type, "survival")
})

test_that("cart_to_plan with intervals output type", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "v", table = "condition_occurrence",
                          concept_id = 201820)
  c <- cart_add_output(c, omop_output(name = "int", type = "intervals"))
  plan <- cart_to_plan(c)
  expect_true("int" %in% names(plan$outputs))
  expect_equal(plan$outputs$int$type, "intervals_long")
})

# --- cart_to_code enhanced ---------------------------------------------------

test_that("cart_to_code includes populations", {
  c <- omop_cart()
  c <- cart_add_population(c,
    omop_population(id = "adults", label = "Adults 18+", parent_id = "base"))
  c <- cart_add_variable(c, name = "v", table = "person")
  code <- cart_to_code(c)
  expect_true(grepl("omop_population", code))
  expect_true(grepl("adults", code))
})

test_that("cart_to_code includes blocks", {
  c <- omop_cart()
  b <- omop_variable_block(
    id = "cond", table = "condition_occurrence",
    concept_ids = c(201820, 4229440), format = "binary"
  )
  c <- cart_add_block(c, b)
  code <- cart_to_code(c)
  expect_true(grepl("omop_variable_block", code))
  expect_true(grepl("cart_add_block", code))
})

test_that("cart_to_code includes filter groups", {
  c <- omop_cart()
  g <- omop_filter_group(
    omop_filter_sex("F"),
    omop_filter_age(18, 65),
    operator = "AND"
  )
  c <- cart_add_filter(c, g)
  code <- cart_to_code(c)
  expect_true(grepl("omop_filter_group", code))
  expect_true(grepl("AND", code))
})

test_that("cart_to_code does not include library calls", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "v", table = "person")
  c <- cart_add_output(c, omop_output(name = "o", type = "wide"))
  code <- cart_to_code(c)
  expect_false(grepl("library", code))
})

test_that("cart_to_code skips base population", {
  c <- omop_cart()
  code <- cart_to_code(c)
  # Should NOT have cart_add_population for base
  expect_false(grepl("cart_add_population.*base", code))
})

test_that("cart_to_code excludes block-generated variables", {
  c <- omop_cart()
  b <- omop_variable_block(
    id = "cond", table = "condition_occurrence",
    concept_ids = c(201820),
    concept_names = c("Diabetes"),
    format = "binary"
  )
  c <- cart_add_block(c, b)
  code <- cart_to_code(c)
  # The block generates a "diabetes" variable, but codegen should NOT
  # separately emit cart_add_variable for it (block handles it)
  expect_true(grepl("cart_add_block", code))
  expect_false(grepl("cart_add_variable.*diabetes", code))
})

# --- cart_export_json / cart_import_json -------------------------------------

test_that("cart_export_json returns JSON string", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "yob", table = "person",
                          column = "year_of_birth")
  c <- cart_add_filter(c, omop_filter_sex("F"))
  c <- cart_add_output(c, omop_output(name = "out", type = "wide"))

  json <- cart_export_json(c)
  expect_true(is.character(json))
  expect_true(nchar(json) > 0)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed$version, "2.0")
  expect_true("yob" %in% names(parsed$variables))
})

test_that("cart_export_json writes to file", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "v", table = "person")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  result <- cart_export_json(c, file = tmp)
  expect_true(file.exists(tmp))
  expect_equal(result, tmp)
})

test_that("cart_import_json roundtrips from string", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "yob", table = "person",
                          column = "year_of_birth", type = "numeric")
  c <- cart_add_variable(c, name = "diabetes", table = "condition_occurrence",
                          concept_id = 201820, format = "binary")
  c <- cart_add_filter(c, omop_filter_sex("F"), id = "sex_f")
  c <- cart_add_output(c, omop_output(name = "data", type = "wide"))

  json <- cart_export_json(c)
  c2 <- cart_import_json(json)

  expect_s3_class(c2, "omop_cart")
  expect_equal(length(c2$variables), 2)
  expect_true("yob" %in% names(c2$variables))
  expect_true("diabetes" %in% names(c2$variables))
  expect_equal(c2$variables$yob$type, "numeric")
  expect_equal(c2$variables$diabetes$concept_id, 201820L)
  expect_equal(length(c2$filters), 1)
  expect_equal(length(c2$outputs), 1)
  expect_equal(c2$outputs$data$type, "wide")
})

test_that("cart_import_json roundtrips from file", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "v", table = "measurement",
                          concept_id = 3004249, format = "mean")
  c <- cart_add_output(c, omop_output(name = "o", type = "long"))

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  cart_export_json(c, file = tmp)
  c2 <- cart_import_json(tmp)

  expect_s3_class(c2, "omop_cart")
  expect_equal(length(c2$variables), 1)
  expect_equal(c2$variables$v$format, "mean")
})

test_that("cart_import_json preserves populations", {
  c <- omop_cart()
  c <- cart_add_population(c,
    omop_population(id = "adults", label = "Adults", parent_id = "base"))

  json <- cart_export_json(c)
  c2 <- cart_import_json(json)

  expect_true("base" %in% names(c2$populations))
  expect_true("adults" %in% names(c2$populations))
  expect_equal(c2$populations$adults$label, "Adults")
})

# --- cart_preview_schema with enhanced outputs --------------------------------

test_that("cart_preview_schema includes population_id attribute", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "v", table = "person")
  c <- cart_add_output(c,
    omop_output(name = "adult_data", type = "wide", population_id = "base"))
  schemas <- cart_preview_schema(c)
  expect_equal(attr(schemas$adult_data, "population_id"), "base")
})

test_that("cart_preview_schema with multiple outputs", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "yob", table = "person",
                          column = "year_of_birth")
  c <- cart_add_variable(c, name = "cond", table = "condition_occurrence",
                          concept_id = 201820)
  c <- cart_add_output(c, omop_output(name = "demo", type = "baseline",
                                       variables = "yob"))
  c <- cart_add_output(c, omop_output(name = "events", type = "long",
                                       variables = "cond"))
  schemas <- cart_preview_schema(c)
  expect_equal(length(schemas), 2)
  expect_equal(nrow(schemas$demo), 1)
  expect_equal(nrow(schemas$events), 1)
  expect_equal(schemas$demo$column, "yob")
  expect_equal(schemas$events$column, "cond")
})

# --- .codegen_filter / .codegen_filter_group ---------------------------------

test_that(".codegen_filter generates correct code for each type", {
  f_sex <- omop_filter_sex("M")
  expect_true(grepl("omop_filter_sex", .codegen_filter(f_sex)))

  f_age <- omop_filter_age(18, 65)
  expect_true(grepl("omop_filter_age", .codegen_filter(f_age)))

  f_hc <- omop_filter_has_concept(201820, "condition_occurrence")
  expect_true(grepl("omop_filter_has_concept", .codegen_filter(f_hc)))

  f_dr <- omop_filter_date_range("2020-01-01", "2023-12-31")
  expect_true(grepl("omop_filter_date_range", .codegen_filter(f_dr)))

  f_val <- omop_filter_value(">=", 6.5)
  expect_true(grepl("omop_filter_value", .codegen_filter(f_val)))
})

test_that(".codegen_filter falls back to generic for unknown types", {
  f <- omop_filter(type = "dedup", level = "output")
  code <- .codegen_filter(f)
  expect_true(grepl("omop_filter", code))
  expect_true(grepl("dedup", code))
})

test_that(".codegen_filter_group generates nested code", {
  g <- omop_filter_group(
    omop_filter_sex("F"),
    omop_filter_group(
      omop_filter_age(18, 65),
      omop_filter_has_concept(201820, "condition_occurrence"),
      operator = "AND"
    ),
    operator = "OR"
  )
  code <- .codegen_filter_group(g)
  expect_true(grepl("omop_filter_group", code))
  expect_true(grepl("OR", code))
  expect_true(grepl("AND", code))
})

# --- cart_clear with enhanced fields -----------------------------------------

test_that("cart_clear resets populations/blocks too", {
  c <- omop_cart()
  c <- cart_add_population(c,
    omop_population(id = "adults", parent_id = "base"))
  c <- cart_add_block(c,
    omop_variable_block(table = "condition_occurrence",
                         concept_ids = c(1, 2)))
  c <- cart_add_filter(c, omop_filter_sex("F"))
  c <- cart_add_output(c, omop_output(name = "o", type = "wide"))

  c <- cart_clear(c)
  expect_equal(length(c$populations), 1)  # only base
  expect_equal(length(c$blocks), 0)
  expect_equal(length(c$variables), 0)
  expect_equal(length(c$filters), 0)
  expect_equal(length(c$outputs), 0)
})

# --- cart print enhanced fields ----------------------------------------------

test_that("cart print shows populations and blocks", {
  c <- omop_cart()
  c <- cart_add_population(c,
    omop_population(id = "adults", label = "Adults 18+", parent_id = "base"))
  b <- omop_variable_block(
    id = "cond_block", table = "condition_occurrence",
    concept_ids = c(201820, 4229440),
    concept_names = c("Diabetes", "Hypertension"),
    format = "binary"
  )
  c <- cart_add_block(c, b)
  c <- cart_add_output(c, omop_output(name = "out", type = "wide"))
  out <- capture.output(print(c))
  expect_true(any(grepl("Populations", out)))
  expect_true(any(grepl("adults", out)))
  expect_true(any(grepl("Variable Blocks", out)))
  expect_true(any(grepl("cond_block", out)))
})

# --- edge cases --------------------------------------------------------------

test_that("cart_to_plan with no outputs returns empty plan", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "v", table = "person")
  plan <- cart_to_plan(c)
  expect_s3_class(plan, "omop_plan")
  expect_equal(length(plan$outputs), 0)
})

test_that("cart_to_plan with output referencing nonexistent variables", {
  c <- omop_cart()
  c <- cart_add_variable(c, name = "a", table = "person")
  c <- cart_add_output(c,
    omop_output(name = "out", type = "wide",
                variables = c("a", "nonexistent")))
  # Should not error, just filters to available variables
  plan <- cart_to_plan(c)
  expect_s3_class(plan, "omop_plan")
})

test_that("cart meta tracks modification time", {
  c <- omop_cart()
  t1 <- c$meta$modified
  Sys.sleep(0.01)
  c <- cart_add_variable(c, name = "v", table = "person")
  t2 <- c$meta$modified
  expect_true(t2 >= t1)
})
