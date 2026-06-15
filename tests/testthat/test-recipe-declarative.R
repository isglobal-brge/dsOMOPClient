# Declarative recipe flexibility battery.
#
# Proves the declarative omop_recipe(...) form can express the full DSL with a
# large, deliberately convoluted set of archetypes spanning every clinical
# table and every feature. For EACH archetype we assert five properties:
#
#   (a) the declarative omop_recipe(...) BUILDS without error;
#   (b) recipe_to_plan() COMPILES it;
#   (c) EQUIVALENCE: the SAME recipe built by calling the now-internal setters
#       directly (omop_recipe() + dsOMOPClient:::recipe_add_*) is identical() to
#       the declarative one (meta timestamps excluded) -- proving the
#       declarative form lost nothing the step-by-step setters have;
#   (d) recipe_export_yaml() -> recipe_import_yaml() round-trips identical;
#   (e) recipe_to_code() emits a declarative omop_recipe(...) that re-evaluates
#       to the same recipe.
#
# recipe_to_plan() is connection-free, so none of this needs a DB.
#
# The internal setters are intentionally unexported (the declarative
# constructor is the single authoring entry point); we reach them via ::: only
# to build the step-by-step control recipe for the equivalence proof.

`%||%` <- function(a, b) if (is.null(a)) a <- b else a

# Drop the volatile meta block (created/modified timestamps) so two recipes
# built at different instants compare on structure alone.
strip_meta <- function(r) { r$meta <- NULL; r }

# The five-way check, returned as a named logical vector so a failing leg is
# obvious in the test output.
recipe_checks <- function(declarative, stepwise) {
  out <- c(
    builds        = inherits(declarative, "omop_recipe"),
    compiles      = inherits(recipe_to_plan(declarative), "omop_plan"),
    identical_set = identical(strip_meta(declarative), strip_meta(stepwise)),
    plan_identical = identical(recipe_to_plan(declarative),
                               recipe_to_plan(stepwise)),
    yaml_roundtrip = identical(
      strip_meta(declarative),
      strip_meta(recipe_import_yaml(recipe_export_yaml(declarative)))),
    code_roundtrip = {
      e <- new.env()
      eval(parse(text = recipe_to_code(declarative)), envir = e)
      identical(strip_meta(e$recipe), strip_meta(declarative))
    }
  )
  out
}

# ---------------------------------------------------------------------------
# The archetype catalog. Each entry is list(decl = <fn>, step = <fn>), where
# both functions return an omop_recipe built the two different ways. Kept side
# by side so the equivalence proof is auditable per archetype.
# ---------------------------------------------------------------------------

archetypes <- list()

add_arch <- function(name, decl, step) {
  archetypes[[name]] <<- list(decl = decl, step = step)
}

# 1. Simplest: age + sex + one condition (binary) -> wide.
add_arch("01_age_sex_condition",
  decl = function() omop_recipe(
    variables = list(
      omop_variable_age(),
      omop_variable_sex(),
      omop_variable(table = "condition_occurrence", concept_id = 201820,
                    concept_name = "Type 2 diabetes", format = "binary")),
    outputs = omop_output(name = "study", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "condition_occurrence", concept_id = 201820,
      concept_name = "Type 2 diabetes", format = "binary"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "study",
                                                         type = "wide"))
    r
  })

# 2. Single sex population filter only.
add_arch("02_sex_filter_only",
  decl = function() omop_recipe(
    filters = omop_filter_sex("F"),
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_sex("F"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 3. Deeply nested AND/OR/NOT group tree (population level).
add_arch("03_nested_and_or_not",
  decl = function() omop_recipe(
    filters = omop_filter_group(
      omop_filter_sex("F"),
      omop_filter_group(
        omop_filter_has_concept(201820, "condition_occurrence",
                                concept_name = "T2DM"),
        omop_filter_group(
          omop_filter_not_has_concept(1124300, "drug_exposure",
                                      concept_name = "Metformin"),
          omop_filter_has_concept(316866, "condition_occurrence",
                                  concept_name = "MI"),
          operator = "OR"),
        operator = "AND"),
      operator = "AND"),
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_group(
      omop_filter_sex("F"),
      omop_filter_group(
        omop_filter_has_concept(201820, "condition_occurrence",
                                concept_name = "T2DM"),
        omop_filter_group(
          omop_filter_not_has_concept(1124300, "drug_exposure",
                                      concept_name = "Metformin"),
          omop_filter_has_concept(316866, "condition_occurrence",
                                  concept_name = "MI"),
          operator = "OR"),
        operator = "AND"),
      operator = "AND"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 4. Value filter via disclosure-safe bins (above) on a measurement, long out.
add_arch("04_value_bin_above",
  decl = function() omop_recipe(
    filters = omop_filter_value("value_as_number", threshold = 7,
                                direction = "above",
                                safe_bins = list(breaks = c(0, 5, 7, 9, 15))),
    variables = omop_variable(table = "measurement", concept_id = 3004410,
                              concept_name = "HbA1c", format = "mean",
                              value_source = "value_as_number"),
    outputs = omop_output(name = "m", type = "long")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_value(
      "value_as_number", threshold = 7, direction = "above",
      safe_bins = list(breaks = c(0, 5, 7, 9, 15))))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "m",
                                                         type = "long"))
    r
  })

# 5. Value filter (below) + value_concept + date window combined.
add_arch("05_value_below_concept_date",
  decl = function() omop_recipe(
    filters = list(
      omop_filter_value("value_as_number", threshold = 5,
                        direction = "below",
                        safe_bins = list(breaks = c(0, 4, 6, 8, 20))),
      omop_filter_value_concept(c(4171373L, 45877994L),
                                concept_name = c("High", "VeryHigh")),
      omop_filter_date_range("2015-01-01", "2022-12-31")),
    variables = omop_variable(table = "measurement", concept_id = 3013682,
                              format = "last_value",
                              value_source = "value_as_number"),
    outputs = omop_output(name = "lab", type = "long")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_value(
      "value_as_number", threshold = 5, direction = "below",
      safe_bins = list(breaks = c(0, 4, 6, 8, 20))))
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_value_concept(
      c(4171373L, 45877994L), concept_name = c("High", "VeryHigh")))
    r <- dsOMOPClient:::recipe_add_filter(r,
      omop_filter_date_range("2015-01-01", "2022-12-31"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3013682, format = "last_value",
      value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "lab",
                                                         type = "long"))
    r
  })

# 6. age_range population filter (narrow) + age_group.
add_arch("06_age_range_group",
  decl = function() omop_recipe(
    filters = list(
      omop_filter_age(40, 75),
      omop_filter_age_group(c("40-49", "50-59", "60-69", "70-74"))),
    variables = omop_variable_sex(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_age(40, 75))
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_age_group(
      c("40-49", "50-59", "60-69", "70-74")))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 7. has_concept with window + min_count, multi-concept vector.
add_arch("07_has_concept_window_mincount",
  decl = function() omop_recipe(
    filters = omop_filter_has_concept(
      c(201820L, 4193704L), "condition_occurrence", concept_name = "Diabetes",
      window = list(start = -9999, end = 0), min_count = 2L),
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_has_concept(
      c(201820L, 4193704L), "condition_occurrence", concept_name = "Diabetes",
      window = list(start = -9999, end = 0), min_count = 2L))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 8. not_has_concept with window (absence in the prior year).
add_arch("08_not_has_concept_window",
  decl = function() omop_recipe(
    filters = omop_filter_not_has_concept(
      1124300, "drug_exposure", concept_name = "Metformin",
      window = list(start = -365, end = 0)),
    variables = omop_variable_sex(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_not_has_concept(
      1124300, "drug_exposure", concept_name = "Metformin",
      window = list(start = -365, end = 0)))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 9. missing_measurement with window + has_measurement range.
add_arch("09_missing_and_has_measurement",
  decl = function() omop_recipe(
    filters = list(
      omop_filter_missing_measurement(c(3004410L, 3013682L),
                                      window = list(start = -180, end = 0)),
      omop_filter_has_measurement(3004410, min_value = 4, max_value = 14)),
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_missing_measurement(
      c(3004410L, 3013682L), window = list(start = -180, end = 0)))
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_has_measurement(
      3004410, min_value = 4, max_value = 14))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 10. prior_observation + followup population filters.
add_arch("10_prior_followup",
  decl = function() omop_recipe(
    filters = list(
      omop_filter_prior_observation(min_days = 730L),
      omop_filter_followup(min_days = 90L)),
    variables = omop_variable_sex(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r,
      omop_filter_prior_observation(min_days = 730L))
    r <- dsOMOPClient:::recipe_add_filter(r,
      omop_filter_followup(min_days = 90L))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 11. visit_count with visit_concept_id vector.
add_arch("11_visit_count",
  decl = function() omop_recipe(
    filters = omop_filter_visit_count(min_count = 2L,
                                      visit_concept_id = c(9201L, 9203L)),
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_visit_count(
      min_count = 2L, visit_concept_id = c(9201L, 9203L)))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 12. concept_count population filter.
add_arch("12_concept_count",
  decl = function() omop_recipe(
    filters = omop_filter_concept_count(316866, "condition_occurrence",
                                        min_count = 3L, concept_name = "MI"),
    variables = omop_variable_sex(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_concept_count(
      316866, "condition_occurrence", min_count = 3L, concept_name = "MI"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 13. Measurement value features: mean + min + max.
add_arch("13_measurement_mean_min_max",
  decl = function() omop_recipe(
    variables = list(
      omop_variable(table = "measurement", concept_id = 3004410,
                    concept_name = "HbA1c mean", format = "mean",
                    value_source = "value_as_number"),
      omop_variable(table = "measurement", concept_id = 3004410,
                    concept_name = "HbA1c min", format = "min",
                    value_source = "value_as_number"),
      omop_variable(table = "measurement", concept_id = 3004410,
                    concept_name = "HbA1c max", format = "max",
                    value_source = "value_as_number")),
    outputs = omop_output(name = "labs", type = "features")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410,
      concept_name = "HbA1c mean", format = "mean",
      value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410,
      concept_name = "HbA1c min", format = "min",
      value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410,
      concept_name = "HbA1c max", format = "max",
      value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "labs",
                                                         type = "features"))
    r
  })

# 14. Quantile-ish dispersion features: sd + cv + slope.
add_arch("14_sd_cv_slope",
  decl = function() omop_recipe(
    variables = list(
      omop_variable_sd("measurement", 3004410, concept_name = "HbA1c"),
      omop_variable_cv("measurement", 3004410, concept_name = "HbA1c"),
      omop_variable_slope("measurement", 3004410, concept_name = "HbA1c")),
    outputs = omop_output(name = "disp", type = "features")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r,
      omop_variable_sd("measurement", 3004410, concept_name = "HbA1c"))
    r <- dsOMOPClient:::recipe_add_variable(r,
      omop_variable_cv("measurement", 3004410, concept_name = "HbA1c"))
    r <- dsOMOPClient:::recipe_add_variable(r,
      omop_variable_slope("measurement", 3004410, concept_name = "HbA1c"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "disp",
                                                         type = "features"))
    r
  })

# 15. Abnormal flags + gap features + duration_sum.
add_arch("15_abnormal_gaps_duration",
  decl = function() omop_recipe(
    variables = list(
      omop_variable(table = "measurement", concept_id = 3004410,
                    format = "abnormal_high", value_source = "value_as_number"),
      omop_variable(table = "measurement", concept_id = 3004410,
                    format = "abnormal_low", value_source = "value_as_number"),
      omop_variable(table = "drug_exposure", concept_id = 1124300,
                    format = "gap_max"),
      omop_variable(table = "drug_exposure", concept_id = 1124300,
                    format = "gap_mean"),
      omop_variable(table = "drug_exposure", concept_id = 1124300,
                    format = "duration_sum")),
    outputs = omop_output(name = "feats", type = "features")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, format = "abnormal_high",
      value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, format = "abnormal_low",
      value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "drug_exposure", concept_id = 1124300, format = "gap_max"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "drug_exposure", concept_id = 1124300, format = "gap_mean"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "drug_exposure", concept_id = 1124300, format = "duration_sum"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "feats",
                                                         type = "features"))
    r
  })

# 16. Multiple tables in one wide output (condition + drug + measurement + proc).
add_arch("16_multi_table_wide",
  decl = function() omop_recipe(
    variables = list(
      omop_variable(table = "condition_occurrence", concept_id = 316866,
                    concept_name = "MI", format = "binary"),
      omop_variable(table = "drug_exposure", concept_id = 1124300,
                    concept_name = "Metformin", format = "count"),
      omop_variable(table = "measurement", concept_id = 3004410,
                    concept_name = "HbA1c", format = "mean",
                    value_source = "value_as_number"),
      omop_variable(table = "procedure_occurrence", concept_id = 4163872,
                    concept_name = "PCI", format = "count")),
    outputs = omop_output(name = "multi", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "condition_occurrence", concept_id = 316866,
      concept_name = "MI", format = "binary"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "drug_exposure", concept_id = 1124300,
      concept_name = "Metformin", format = "count"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "procedure_occurrence", concept_id = 4163872,
      concept_name = "PCI", format = "count"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "multi",
                                                         type = "wide"))
    r
  })

# 17. Multiple outputs: wide + long + features over the same variable pool.
add_arch("17_multi_output_mixed",
  decl = function() omop_recipe(
    variables = list(
      omop_variable_age(),
      omop_variable(table = "condition_occurrence", concept_id = 201820,
                    concept_name = "T2DM", format = "binary"),
      omop_variable(table = "measurement", concept_id = 3004410,
                    concept_name = "HbA1c", format = "mean",
                    value_source = "value_as_number")),
    outputs = list(
      omop_output(name = "wide_all", type = "wide"),
      omop_output(name = "long_meas", type = "long", variables = c("hba1c")),
      omop_output(name = "feat_cond", type = "features", variables = c("t2dm")))),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "condition_occurrence", concept_id = 201820,
      concept_name = "T2DM", format = "binary"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "wide_all",
                                                         type = "wide"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(
      name = "long_meas", type = "long", variables = c("hba1c")))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(
      name = "feat_cond", type = "features", variables = c("t2dm")))
    r
  })

# 18. Survival output with explicit TAR.
add_arch("18_survival_tar",
  decl = function() omop_recipe(
    variables = omop_variable(table = "condition_occurrence", concept_id = 316866,
                              concept_name = "MI", format = "binary"),
    outputs = omop_output(name = "surv", type = "survival",
                          options = list(tar = list(start_offset = 0,
                                                    end_offset = 365)))),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "condition_occurrence", concept_id = 316866,
      concept_name = "MI", format = "binary"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(
      name = "surv", type = "survival",
      options = list(tar = list(start_offset = 0, end_offset = 365))))
    r
  })

# 19. Intervals output across drug + condition.
add_arch("19_intervals",
  decl = function() omop_recipe(
    variables = list(
      omop_variable(table = "drug_exposure", concept_id = 1124300,
                    concept_name = "Metformin", format = "raw"),
      omop_variable(table = "condition_occurrence", concept_id = 201820,
                    concept_name = "T2DM", format = "raw")),
    outputs = omop_output(name = "iv", type = "intervals")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "drug_exposure", concept_id = 1124300,
      concept_name = "Metformin", format = "raw"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "condition_occurrence", concept_id = 201820,
      concept_name = "T2DM", format = "raw"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "iv",
                                                         type = "intervals"))
    r
  })

# 20. covariates_sparse over multiple tables.
add_arch("20_covariates_sparse",
  decl = function() omop_recipe(
    variables = list(
      omop_variable(table = "condition_occurrence", concept_id = 201820,
                    concept_name = "T2DM", format = "binary"),
      omop_variable(table = "drug_exposure", concept_id = 1124300,
                    concept_name = "Metformin", format = "count")),
    outputs = omop_output(name = "sparse", type = "covariates_sparse")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "condition_occurrence", concept_id = 201820,
      concept_name = "T2DM", format = "binary"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "drug_exposure", concept_id = 1124300,
      concept_name = "Metformin", format = "count"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(
      name = "sparse", type = "covariates_sparse"))
    r
  })

# 21. baseline output (person demographics).
add_arch("21_baseline",
  decl = function() omop_recipe(
    variables = list(
      omop_variable(table = "person", column = "gender_concept_id"),
      omop_variable(table = "person", column = "year_of_birth")),
    outputs = omop_output(name = "base_demo", type = "baseline")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "person", column = "gender_concept_id"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "person", column = "year_of_birth"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(
      name = "base_demo", type = "baseline"))
    r
  })

# 22. Variable block (binary) + standalone vars (block re-expansion + order).
add_arch("22_block_plus_vars",
  decl = function() omop_recipe(
    blocks = omop_variable_block(
      table = "condition_occurrence",
      concept_ids = c(201820L, 320128L, 316866L),
      concept_names = c("T2DM", "HTN", "MI"), format = "binary"),
    variables = list(omop_variable_age(), omop_variable_sex()),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_block(r, omop_variable_block(
      table = "condition_occurrence",
      concept_ids = c(201820L, 320128L, 316866L),
      concept_names = c("T2DM", "HTN", "MI"), format = "binary"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 23. Variable block with time_window + row filters + expand descendants.
add_arch("23_block_window_expand_filters",
  decl = function() omop_recipe(
    blocks = omop_variable_block(
      table = "drug_exposure", concept_ids = c(1124300L, 1503297L),
      concept_names = c("Metformin", "Glipizide"), format = "count",
      time_window = list(start = -730, end = 0), expand = TRUE,
      filters = list(omop_filter_date_range("2018-01-01", "2022-12-31"))),
    outputs = omop_output(name = "drugs", type = "features")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_block(r, omop_variable_block(
      table = "drug_exposure", concept_ids = c(1124300L, 1503297L),
      concept_names = c("Metformin", "Glipizide"), format = "count",
      time_window = list(start = -730, end = 0), expand = TRUE,
      filters = list(omop_filter_date_range("2018-01-01", "2022-12-31"))))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "drugs",
                                                         type = "features"))
    r
  })

# 24. Cohort scoping via cohort= (a scalar cohort_definition_id is SCOPE).
add_arch("24_cohort_scope",
  decl = function() omop_recipe(
    variables = list(omop_variable_age(), omop_variable_sex()),
    outputs = omop_output(name = "w", type = "wide"),
    cohort = 42L),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r <- dsOMOPClient:::recipe_set_scope(r, cohort = 42L)
    r
  })

# 25. Temporal covariates: per-variable time_window forwarded to events.
add_arch("25_temporal_covariates",
  decl = function() omop_recipe(
    variables = omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number",
      time_window = list(start = -365, end = 0)),
    outputs = omop_output(name = "cov", type = "long")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number",
      time_window = list(start = -365, end = 0)))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "cov",
                                                         type = "long"))
    r
  })

# 26. visit_filter + concept_col scoping on a variable.
add_arch("26_visit_filter_concept_col",
  decl = function() omop_recipe(
    variables = omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number",
      visit_filter = list(concept_ids = c(9201L, 9203L)),
      concept_col = "unit_concept_id"),
    outputs = omop_output(name = "cov", type = "long",
                          options = list(date_handling = "first"))),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number",
      visit_filter = list(concept_ids = c(9201L, 9203L)),
      concept_col = "unit_concept_id"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(
      name = "cov", type = "long", options = list(date_handling = "first")))
    r
  })

# 27. Comorbidity scores: charlson + chads2 + chadsvasc.
add_arch("27_scores_charlson_chads",
  decl = function() omop_recipe(
    variables = list(
      omop_variable_charlson(),
      omop_variable_chads2(),
      omop_variable_chadsvasc()),
    outputs = omop_output(name = "scores", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_charlson())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_chads2())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_chadsvasc())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "scores",
                                                         type = "wide"))
    r
  })

# 28. Comorbidity scores: dcsi + hfrs (vocabulary-mapped).
add_arch("28_scores_dcsi_hfrs",
  decl = function() omop_recipe(
    variables = list(omop_variable_dcsi(), omop_variable_hfrs()),
    outputs = omop_output(name = "scores2", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_dcsi())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_hfrs())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "scores2",
                                                         type = "wide"))
    r
  })

# 29. Derived durations: obs_duration + prior_obs + followup + demo_missingness.
add_arch("29_derived_durations",
  decl = function() omop_recipe(
    variables = list(
      omop_variable_obs_duration(),
      omop_variable_prior_obs(),
      omop_variable_followup(),
      omop_variable_demo_missingness()),
    outputs = omop_output(name = "dur", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_obs_duration())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_prior_obs())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_followup())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_demo_missingness())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "dur",
                                                         type = "wide"))
    r
  })

# 30. sum + n_distinct + drug_duration derived features.
add_arch("30_sum_ndistinct_drugduration",
  decl = function() omop_recipe(
    variables = list(
      omop_variable_sum("drug_exposure", "days_supply", concept_id = 1124300,
                        concept_name = "Metformin"),
      omop_variable_n_distinct("condition_occurrence"),
      omop_variable_drug_duration(1124300, concept_name = "Metformin",
                                  agg = "max")),
    outputs = omop_output(name = "agg", type = "features")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sum(
      "drug_exposure", "days_supply", concept_id = 1124300,
      concept_name = "Metformin"))
    r <- dsOMOPClient:::recipe_add_variable(r,
      omop_variable_n_distinct("condition_occurrence"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_drug_duration(
      1124300, concept_name = "Metformin", agg = "max"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "agg",
                                                         type = "features"))
    r
  })

# 31. age (index reference) needs cohort -> cohort + age(index).
add_arch("31_age_index_with_cohort",
  decl = function() omop_recipe(
    variables = list(omop_variable_age(reference = "index"),
                     omop_variable_sex()),
    outputs = omop_output(name = "w", type = "wide"),
    cohort = 7L),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r,
      omop_variable_age(reference = "index"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r <- dsOMOPClient:::recipe_set_scope(r, cohort = 7L)
    r
  })

# 32. Named filters list -> names become filter IDs.
add_arch("32_named_filter_ids",
  decl = function() omop_recipe(
    filters = list(
      women = omop_filter_sex("F"),
      adults = omop_filter_age(18, 65),
      diabetic = omop_filter_has_concept(201820, "condition_occurrence",
                                         concept_name = "T2DM")),
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_sex("F"),
                                          id = "women")
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_age(18, 65),
                                          id = "adults")
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_has_concept(
      201820, "condition_occurrence", concept_name = "T2DM"),
      id = "diabetic")
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 33. Plan options override (translate/factor off).
add_arch("33_options_override",
  decl = function() omop_recipe(
    variables = omop_variable_sex(),
    outputs = omop_output(name = "w", type = "wide"),
    options = list(translate_concepts = FALSE, factor_concepts = FALSE)),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r <- dsOMOPClient:::recipe_set_options(r, translate_concepts = FALSE,
                                           factor_concepts = FALSE)
    r
  })

# 34. block_sensitive option override only.
add_arch("34_options_block_sensitive",
  decl = function() omop_recipe(
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide"),
    options = list(block_sensitive = FALSE)),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r <- dsOMOPClient:::recipe_set_options(r, block_sensitive = FALSE)
    r
  })

# 35. Per-variable row filters (value_bin + date_range) attached to a variable.
add_arch("35_per_variable_row_filters",
  decl = function() omop_recipe(
    variables = omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number",
      filters = list(
        omop_filter_date_range("2019-01-01", "2022-12-31"),
        omop_filter_value("value_as_number", threshold = 6,
                          direction = "above",
                          safe_bins = list(breaks = c(0, 4, 6, 8, 12))))),
    outputs = omop_output(name = "labs", type = "long")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number",
      filters = list(
        omop_filter_date_range("2019-01-01", "2022-12-31"),
        omop_filter_value("value_as_number", threshold = 6,
                          direction = "above",
                          safe_bins = list(breaks = c(0, 4, 6, 8, 12))))))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "labs",
                                                         type = "long"))
    r
  })

# 36. The MAINTAINER example: women with condX AND measHigh, UNION women with
#     condY AND measLow, then of those those with measZ, combining drugs/visits/
#     dates. Set-ops/UNION are not in the DSL, so the OR-of-AND groups express
#     the richest single-cohort approximation; the drug count/duration variables
#     and the date/visit constraints all live in one declarative call.
add_arch("36_maintainer_example",
  decl = function() omop_recipe(
    filters = list(
      main = omop_filter_group(
        omop_filter_sex("F"),
        omop_filter_group(
          omop_filter_group(
            omop_filter_has_concept(316866, "condition_occurrence",
                                    concept_name = "condX"),
            omop_filter_has_measurement(3004410, min_value = 7.0),
            operator = "AND"),
          omop_filter_group(
            omop_filter_has_concept(201820, "condition_occurrence",
                                    concept_name = "condY"),
            omop_filter_has_measurement(3004410, max_value = 5.0),
            operator = "AND"),
          operator = "OR"),
        omop_filter_has_measurement(3013682),
        omop_filter_date_range("2016-01-01", "2022-12-31"),
        omop_filter_visit_count(min_count = 1L),
        operator = "AND")),
    variables = list(
      omop_variable(table = "drug_exposure", concept_id = 1124300,
                    concept_name = "drugA", format = "count"),
      omop_variable_drug_duration(1124300, concept_name = "drugA",
                                  agg = "sum")),
    outputs = omop_output(name = "cohort", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_group(
      omop_filter_sex("F"),
      omop_filter_group(
        omop_filter_group(
          omop_filter_has_concept(316866, "condition_occurrence",
                                  concept_name = "condX"),
          omop_filter_has_measurement(3004410, min_value = 7.0),
          operator = "AND"),
        omop_filter_group(
          omop_filter_has_concept(201820, "condition_occurrence",
                                  concept_name = "condY"),
          omop_filter_has_measurement(3004410, max_value = 5.0),
          operator = "AND"),
        operator = "OR"),
      omop_filter_has_measurement(3013682),
      omop_filter_date_range("2016-01-01", "2022-12-31"),
      omop_filter_visit_count(min_count = 1L),
      operator = "AND"), id = "main")
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "drug_exposure", concept_id = 1124300, concept_name = "drugA",
      format = "count"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_drug_duration(
      1124300, concept_name = "drugA", agg = "sum"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "cohort",
                                                         type = "wide"))
    r
  })

# 37. WILDER: 4-level nested tree (AND(OR(AND(OR)))) mixing every population
#     filter type, plus a multi-table feature output.
add_arch("37_four_level_tree",
  decl = function() omop_recipe(
    filters = omop_filter_group(
      omop_filter_age(18, 90),
      omop_filter_group(
        omop_filter_group(
          omop_filter_has_concept(201820, "condition_occurrence",
                                  concept_name = "T2DM"),
          omop_filter_group(
            omop_filter_concept_count(316866, "condition_occurrence",
                                      min_count = 2L),
            omop_filter_not_has_concept(440383, "condition_occurrence",
                                        concept_name = "Depression"),
            operator = "OR"),
          operator = "AND"),
        omop_filter_group(
          omop_filter_prior_observation(min_days = 365L),
          omop_filter_visit_count(min_count = 3L),
          operator = "AND"),
        operator = "OR"),
      operator = "AND"),
    variables = list(
      omop_variable(table = "measurement", concept_id = 3004410,
                    concept_name = "HbA1c", format = "mean",
                    value_source = "value_as_number"),
      omop_variable(table = "drug_exposure", concept_id = 1124300,
                    concept_name = "Metformin", format = "count")),
    outputs = omop_output(name = "feat", type = "features")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_group(
      omop_filter_age(18, 90),
      omop_filter_group(
        omop_filter_group(
          omop_filter_has_concept(201820, "condition_occurrence",
                                  concept_name = "T2DM"),
          omop_filter_group(
            omop_filter_concept_count(316866, "condition_occurrence",
                                      min_count = 2L),
            omop_filter_not_has_concept(440383, "condition_occurrence",
                                        concept_name = "Depression"),
            operator = "OR"),
          operator = "AND"),
        omop_filter_group(
          omop_filter_prior_observation(min_days = 365L),
          omop_filter_visit_count(min_count = 3L),
          operator = "AND"),
        operator = "OR"),
      operator = "AND"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "drug_exposure", concept_id = 1124300,
      concept_name = "Metformin", format = "count"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "feat",
                                                         type = "features"))
    r
  })

# 38. KITCHEN SINK: many tables, blocks, derived, scores, multi-output,
#     population filters, options, cohort, temporal -- all in one declarative
#     call. The single most convoluted single-cohort recipe in the battery.
add_arch("38_kitchen_sink",
  decl = function() omop_recipe(
    blocks = list(
      omop_variable_block(table = "condition_occurrence",
                          concept_ids = c(201820L, 320128L),
                          concept_names = c("T2DM", "HTN"), format = "binary"),
      omop_variable_block(table = "drug_exposure",
                          concept_ids = c(1124300L, 1503297L),
                          concept_names = c("Metformin", "Glipizide"),
                          format = "count",
                          time_window = list(start = -365, end = 0))),
    variables = list(
      omop_variable_age(),
      omop_variable_sex(),
      omop_variable_charlson(),
      omop_variable(table = "measurement", concept_id = 3004410,
                    concept_name = "HbA1c", format = "mean",
                    value_source = "value_as_number",
                    time_window = list(start = -180, end = 0)),
      omop_variable(table = "procedure_occurrence", concept_id = 4163872,
                    concept_name = "PCI", format = "count")),
    filters = list(
      omop_filter_age(40, 85),
      omop_filter_group(
        omop_filter_has_concept(201820, "condition_occurrence",
                                concept_name = "T2DM"),
        omop_filter_followup(min_days = 180L),
        operator = "AND")),
    outputs = list(
      omop_output(name = "wide_all", type = "wide"),
      omop_output(name = "feat_drugs", type = "features",
                  variables = c("metformin", "glipizide"))),
    cohort = 99L,
    options = list(translate_concepts = TRUE, block_sensitive = FALSE,
                   factor_concepts = TRUE)),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_block(r, omop_variable_block(
      table = "condition_occurrence", concept_ids = c(201820L, 320128L),
      concept_names = c("T2DM", "HTN"), format = "binary"))
    r <- dsOMOPClient:::recipe_add_block(r, omop_variable_block(
      table = "drug_exposure", concept_ids = c(1124300L, 1503297L),
      concept_names = c("Metformin", "Glipizide"), format = "count",
      time_window = list(start = -365, end = 0)))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_age())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_sex())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable_charlson())
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "mean", value_source = "value_as_number",
      time_window = list(start = -180, end = 0)))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "procedure_occurrence", concept_id = 4163872,
      concept_name = "PCI", format = "count"))
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_age(40, 85))
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter_group(
      omop_filter_has_concept(201820, "condition_occurrence",
                              concept_name = "T2DM"),
      omop_filter_followup(min_days = 180L),
      operator = "AND"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "wide_all",
                                                         type = "wide"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(
      name = "feat_drugs", type = "features",
      variables = c("metformin", "glipizide")))
    r <- dsOMOPClient:::recipe_set_scope(r, cohort = 99L)
    r <- dsOMOPClient:::recipe_set_options(r, translate_concepts = TRUE,
                                           block_sensitive = FALSE,
                                           factor_concepts = TRUE)
    r
  })

# 39. Observation + device_exposure + specimen tables (rarer domains).
add_arch("39_rare_domains",
  decl = function() omop_recipe(
    variables = list(
      omop_variable(table = "observation", concept_id = 4058243,
                    concept_name = "Smoker", format = "binary"),
      omop_variable(table = "device_exposure", concept_id = 4206863,
                    concept_name = "Stent", format = "count"),
      omop_variable(table = "specimen", concept_id = 4001225,
                    concept_name = "Blood specimen", format = "count")),
    outputs = omop_output(name = "rare", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "observation", concept_id = 4058243, concept_name = "Smoker",
      format = "binary"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "device_exposure", concept_id = 4206863, concept_name = "Stent",
      format = "count"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "specimen", concept_id = 4001225,
      concept_name = "Blood specimen", format = "count"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "rare",
                                                         type = "wide"))
    r
  })

# 40. time_since + first_value + count over a single condition (mixed formats).
add_arch("40_time_since_firstvalue_count",
  decl = function() omop_recipe(
    variables = list(
      omop_variable(table = "condition_occurrence", concept_id = 316866,
                    concept_name = "MI since", format = "time_since"),
      omop_variable(table = "measurement", concept_id = 3004410,
                    concept_name = "HbA1c first", format = "first_value",
                    value_source = "value_as_number"),
      omop_variable(table = "condition_occurrence", concept_id = 316866,
                    concept_name = "MI count", format = "count")),
    outputs = omop_output(name = "mixed", type = "features")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "condition_occurrence", concept_id = 316866,
      concept_name = "MI since", format = "time_since"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410,
      concept_name = "HbA1c first", format = "first_value",
      value_source = "value_as_number"))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "condition_occurrence", concept_id = 316866,
      concept_name = "MI count", format = "count"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "mixed",
                                                         type = "features"))
    r
  })

# 41. dedup / min_count / top_n output-level + custom filter (generic types).
add_arch("41_generic_filter_types",
  decl = function() omop_recipe(
    filters = list(
      omop_filter(type = "min_count", level = "population",
                  params = list(min_count = 2L)),
      omop_filter(type = "dedup", level = "output", params = list()),
      omop_filter(type = "top_n", level = "output", params = list(n = 100L))),
    variables = omop_variable(table = "condition_occurrence",
                              concept_id = 201820, concept_name = "T2DM",
                              format = "binary"),
    outputs = omop_output(name = "w", type = "wide")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter(
      type = "min_count", level = "population",
      params = list(min_count = 2L)))
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter(
      type = "dedup", level = "output", params = list()))
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter(
      type = "top_n", level = "output", params = list(n = 100L)))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "condition_occurrence", concept_id = 201820,
      concept_name = "T2DM", format = "binary"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "w",
                                                         type = "wide"))
    r
  })

# 42. concept_set row filter + binned format + suffix_mode label.
add_arch("42_concept_set_binned",
  decl = function() omop_recipe(
    filters = omop_filter(type = "concept_set", level = "row",
                          params = list(concept_ids = c(201820L, 320128L),
                                        concept_col = "condition_concept_id")),
    variables = omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "binned", value_source = "value_as_number",
      suffix_mode = "label"),
    outputs = omop_output(name = "binned_out", type = "long")),
  step = function() {
    r <- omop_recipe()
    r <- dsOMOPClient:::recipe_add_filter(r, omop_filter(
      type = "concept_set", level = "row",
      params = list(concept_ids = c(201820L, 320128L),
                    concept_col = "condition_concept_id")))
    r <- dsOMOPClient:::recipe_add_variable(r, omop_variable(
      table = "measurement", concept_id = 3004410, concept_name = "HbA1c",
      format = "binned", value_source = "value_as_number",
      suffix_mode = "label"))
    r <- dsOMOPClient:::recipe_add_output(r, omop_output(name = "binned_out",
                                                         type = "long"))
    r
  })

# ---------------------------------------------------------------------------
# Drive the battery: one test per archetype, asserting all five legs.
# ---------------------------------------------------------------------------

test_that("declarative archetype battery covers >= 40 archetypes", {
  expect_gte(length(archetypes), 40)
})

for (nm in names(archetypes)) {
  local({
    arch_name <- nm
    arch <- archetypes[[nm]]
    test_that(paste0("declarative archetype: ", arch_name), {
      declarative <- arch$decl()
      stepwise <- arch$step()
      checks <- recipe_checks(declarative, stepwise)
      # Each leg must hold; report which one failed by name.
      for (leg in names(checks)) {
        expect_true(checks[[leg]],
                    info = paste0(arch_name, " :: ", leg, " failed"))
      }
    })
  })
}

# ---------------------------------------------------------------------------
# MULTI-POPULATION: archetypes whose outputs target non-base populations. The
# declarative constructor BUILDS them, they are identical() to the step-by-step
# build, AND they now COMPILE -- recipe_to_plan serializes every population into
# plan$populations and stamps each output with its population_id.
# ---------------------------------------------------------------------------

test_that("multi-population DAG builds, is identical, and compiles", {
  decl <- omop_recipe(
    populations = list(
      omop_population(id = "diab", label = "Diabetics", parent_id = "base",
                      filters = list(omop_filter_has_concept(
                        201820, "condition_occurrence"))),
      omop_population(id = "diab_f", label = "Female diabetics",
                      parent_id = "diab",
                      filters = list(omop_filter_sex("F")))),
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide", population_id = "diab_f"))

  step <- omop_recipe()
  step <- dsOMOPClient:::recipe_add_population(step, omop_population(
    id = "diab", label = "Diabetics", parent_id = "base",
    filters = list(omop_filter_has_concept(201820, "condition_occurrence"))))
  step <- dsOMOPClient:::recipe_add_population(step, omop_population(
    id = "diab_f", label = "Female diabetics", parent_id = "diab",
    filters = list(omop_filter_sex("F"))))
  step <- dsOMOPClient:::recipe_add_variable(step, omop_variable_age())
  step <- dsOMOPClient:::recipe_add_output(step, omop_output(
    name = "w", type = "wide", population_id = "diab_f"))

  # (a) declarative BUILDS, (c) identical to step-by-step, (d) yaml round-trips.
  expect_s3_class(decl, "omop_recipe")
  expect_equal(length(decl$populations), 3L)
  expect_identical(strip_meta(decl), strip_meta(step))
  expect_identical(strip_meta(decl),
                   strip_meta(recipe_import_yaml(recipe_export_yaml(decl))))
  # (b) COMPILE now succeeds for BOTH forms and serializes every population.
  plan_d <- recipe_to_plan(decl)
  plan_s <- recipe_to_plan(step)
  expect_identical(plan_d$populations, plan_s$populations)
  expect_setequal(names(plan_d$populations), c("base", "diab", "diab_f"))
  expect_identical(plan_d$populations$diab$kind, "criteria")
  expect_false(is.null(plan_d$populations$diab$filter_tree))
  # The single output is stamped with the population it targets.
  expect_identical(plan_d$outputs[[names(plan_d$outputs)[1]]]$population_id,
                   "diab_f")
})

test_that("cross-population output target compiles and is stamped", {
  # An output targeting a declared non-base population compiles; the population
  # is serialized and the output carries its population_id.
  decl <- omop_recipe(
    populations = omop_population(id = "sub", label = "Sub", parent_id = "base",
                                  filters = list(omop_filter_sex("F"))),
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide", population_id = "sub"))
  expect_s3_class(decl, "omop_recipe")
  plan <- recipe_to_plan(decl)
  expect_setequal(names(plan$populations), c("base", "sub"))
  expect_identical(plan$populations$sub$kind, "criteria")
  expect_identical(plan$outputs[[names(plan$outputs)[1]]]$population_id, "sub")
})

test_that("set-op population serializes and an output can target it", {
  decl <- omop_recipe(
    populations = list(
      omop_population("diab", "Diabetics",
                      filters = list(omop_filter_has_concept(
                        201820, "condition_occurrence"))),
      omop_population("hyp", "Hypertensives",
                      filters = list(omop_filter_has_concept(
                        320128, "condition_occurrence"))),
      omop_population("either", "Diabetic or hypertensive",
                      union = c("diab", "hyp"))),
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide", population_id = "either"))

  expect_false(is.null(decl$populations$either$setop))
  expect_identical(decl$populations$either$setop$op, "union")
  expect_identical(decl$populations$either$setop$members, c("diab", "hyp"))

  plan <- recipe_to_plan(decl)
  expect_identical(plan$populations$either$kind, "setop")
  expect_identical(plan$populations$either$setop$op, "union")
  expect_identical(plan$populations$either$setop$members, c("diab", "hyp"))
  expect_identical(plan$outputs[[names(plan$outputs)[1]]]$population_id,
                   "either")
  # yaml round-trips the set-op population.
  expect_identical(strip_meta(decl),
                   strip_meta(recipe_import_yaml(recipe_export_yaml(decl))))
})

test_that("recipe-level scope serializes into plan$scope", {
  decl <- omop_recipe(
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide"),
    cohort = "scope_tbl", tables = c("inc_a", "inc_b"), combine = "intersect")
  expect_false(is.null(decl$scope))
  expect_identical(decl$scope$cohort, "scope_tbl")
  expect_identical(decl$scope$tables, c("inc_a", "inc_b"))
  expect_identical(decl$scope$combine, "intersect")
  # A string/handle cohort is scope, NOT the base population cohort.
  expect_null(decl$populations$base$cohort_definition_id)

  plan <- recipe_to_plan(decl)
  expect_identical(plan$scope$cohort, "scope_tbl")
  # tables stay a character vector: ds.omop.plan.execute splices the symbol names
  # into the DataSHIELD call (they cannot ride in the plan JSON).
  expect_identical(plan$scope$tables, c("inc_a", "inc_b"))
  expect_identical(plan$scope$combine, "intersect")
  expect_identical(strip_meta(decl),
                   strip_meta(recipe_import_yaml(recipe_export_yaml(decl))))
})

test_that("output targeting an undeclared population fails closed", {
  expect_error(
    recipe_to_plan(omop_recipe(
      variables = omop_variable_age(),
      outputs = omop_output(name = "w", type = "wide",
                            population_id = "ghost"))),
    "undeclared population")
})
