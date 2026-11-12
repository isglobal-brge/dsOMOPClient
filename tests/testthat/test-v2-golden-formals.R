# WS-8 golden test (design-2.10.3 sections 2 and 10.1): every exported 2.10.1
# function keeps its 2.10.1 positional formals; 2.10.3 arguments are only
# appended, so positional legacy calls keep their meaning. The snapshot below
# is the formals of the 2.10.1 exports (design/src/dsOMOPClient).

.formals_2101 <- list(ds.omop.achilles.analyses = c("domain", "symbol", "conns", 
"type"), ds.omop.achilles.catalog = c("symbol", "conns", "type"
), ds.omop.achilles.distribution = c("analysis_ids", "scope", 
"pooling_policy", "symbol", "conns", "execute", "type"), ds.omop.achilles.heel = c("symbol", 
"conns"), ds.omop.achilles.results = c("analysis_ids", "scope", 
"pooling_policy", "symbol", "conns", "execute", "type"), ds.omop.achilles.status = c("symbol", 
"conns", "type"), ds.omop.analysis.get = c("name", "symbol", 
"conns", "type"), ds.omop.analysis.list = c("domain", "symbol", 
"conns", "type"), ds.omop.analysis.run = c("name", "params", 
"cohort", "tables", "combine", "pooling_policy", "plot", "date_handling", 
"symbol", "conns", "type"), ds.omop.bind_rows = c("x", "y", "newobj", 
"symbol", "conns"), ds.omop.cdm.source = c("symbol", "conns", 
"execute"), ds.omop.cdm.version = c("symbol", "conns", "execute"
), ds.omop.client.version = NULL, ds.omop.code = "x", ds.omop.cohort.combine = c("op", 
"cohort_a", "cohort_b", "new_name", "symbol", "conns"), ds.omop.cohort.create = c("spec", 
"mode", "cohort_id", "name", "overwrite", "symbol", "conns"), 
    ds.omop.cohort.definition = c("id", "symbol", "conns"), ds.omop.cohort.from_table = c("x", 
    "new_name", "symbol", "conns"), ds.omop.cohort.list = c("symbol", 
    "conns"), ds.omop.cohort.ref = "cohort_definition_id", ds.omop.column.stats = c("table", 
    "column", "concept_id", "cohort", "scope", "pooling_policy", 
    "symbol", "conns", "execute", "type"), ds.omop.columns = c("table", 
    "symbol", "conns"), ds.omop.comorbidity = c("conceptA", "conceptB", 
    "tableA", "tableB", "..."), ds.omop.compare = c("symbol", 
    "conns", "tables"), ds.omop.concept.ancestors = c("concept_ids", 
    "symbol", "conns", "execute"), ds.omop.concept.descendants = c("ancestor_ids", 
    "include_self", "symbol", "conns", "execute"), ds.omop.concept.drilldown = c("table", 
    "concept_id", "concept_col", "scope", "pooling_policy", "symbol", 
    "conns", "execute", "type"), ds.omop.concept.expand = c("concept_set", 
    "symbol", "conns", "execute"), ds.omop.concept.list = c("domain", 
    "vocabulary", "concept_class", "standard", "valid", "offset", 
    "limit", "order", "symbol", "conns", "execute"), ds.omop.concept.locate = c("concept_ids", 
    "scope", "pooling_policy", "symbol", "conns", "execute", 
    "type"), ds.omop.concept.lookup = c("concept_ids", "symbol", 
    "conns", "execute"), ds.omop.concept.prevalence = c("table", 
    "concept_col", "metric", "top_n", "cohort_table", "window", 
    "offset", "global", "cohort", "scope", "pooling_policy", 
    "symbol", "conns", "execute", "type"), ds.omop.concept.relationships = c("concept_ids", 
    "relationship_id", "symbol", "conns", "execute"), ds.omop.concept.search = c("pattern", 
    "domain", "vocabulary", "standard_only", "limit", "concept_id", 
    "standard", "valid", "symbol", "conns", "execute"), ds.omop.concept.set = c("concepts", 
    "include_descendants", "include_mapped", "exclude"), ds.omop.concept.summary = c("table", 
    "concept_id", "column", "scope", "pooling_policy", "symbol", 
    "conns", "type"), ds.omop.concept.synonyms = c("concept_ids", 
    "symbol", "conns", "execute"), ds.omop.connect = c("resource", 
    "symbol", "cdm_schema", "vocab_schema", "results_schema", 
    "temp_schema", "strict", "conns"), ds.omop.copy_code = "x", 
    ds.omop.crosstab = c("table", "row", "col", "by", "row_concept_id", 
    "col_concept_id", "cohort_table", "stratify_by", "cohort", 
    "scope", "pooling_policy", "symbol", "conns", "execute", 
    "type"), ds.omop.date.counts = c("table", "date_col", "granularity", 
    "cohort_table", "window", "scope", "pooling_policy", "symbol", 
    "conns", "execute", "type"), ds.omop.disclosure.settings = c("symbol", 
    "conns"), ds.omop.disconnect = c("symbol", "conns"), ds.omop.distribution = c("cohort", 
    "metric", "domain", "top_n", "concept_id", "tables", "plot", 
    "symbol", "conns", "type"), ds.omop.domain.coverage = c("scope", 
    "pooling_policy", "symbol", "conns", "execute", "type"), 
    ds.omop.dp.prepare = c("x", "out", "symbol", "conns"), ds.omop.dp.release = c("x", 
    "privacy", "datasources", "pool", "format", "type"), ds.omop.dp.status = "datasources", 
    ds.omop.filter = c("x", "var", "op", "value", "newobj", "symbol", 
    "conns"), ds.omop.joins = c("symbol", "conns"), ds.omop.login = c("url", 
    "user", "password", "resource", "server", "driver", "token", 
    "profile", "symbol", "..."), ds.omop.merge = c("x", "y", 
    "by", "type", "newobj", "symbol", "conns"), ds.omop.meta.effect_estimate = c("name", 
    "params", "cohort", "tables", "combine", "pooling_policy", 
    "symbol", "conns", "type"), ds.omop.missingness = c("table", 
    "columns", "cohort", "scope", "pooling_policy", "symbol", 
    "conns", "execute", "type"), ds.omop.ohdsi.results = c("table_name", 
    "columns", "filters", "order_by", "limit", "tool_id", "scope", 
    "pooling_policy", "symbol", "conns", "type"), ds.omop.ohdsi.status = c("symbol", 
    "conns", "type"), ds.omop.ohdsi.summary = c("tool_id", "symbol", 
    "conns", "type"), ds.omop.ohdsi.tables = c("symbol", "conns", 
    "type"), ds.omop.plan = NULL, ds.omop.plan.baseline = c("plan", 
    "columns", "derived", "name"), ds.omop.plan.cohort = c("plan", 
    "cohort_definition_id", "spec"), ds.omop.plan.cohort_membership = c("plan", 
    "name"), ds.omop.plan.concept_dictionary = c("plan", "source_outputs", 
    "name"), ds.omop.plan.events = c("plan", "name", "table", 
    "columns", "concept_set", "time_window", "temporal", "date_handling", 
    "filters", "visit_filter", "concept_col", "representation"
    ), ds.omop.plan.execute = c("plan", "out", "symbol", "conns", 
    "output_mode"), ds.omop.plan.features = c("plan", "name", 
    "table", "specs", "grain", "temporal"), ds.omop.plan.harmonize = c("plan", 
    "mode", "strict", "symbol", "conns"), ds.omop.plan.intervals = c("plan", 
    "tables", "concept_filter", "filters", "window", "interval_match", 
    "event_select", "select_n", "select_by", "anchor", "name"
    ), ds.omop.plan.load = "file", ds.omop.plan.options = c("plan", 
    "translate_concepts", "block_sensitive", "factor_concepts"
    ), ds.omop.plan.outcome = c("plan", "name", "concept_set", 
    "table"), ds.omop.plan.person_level = c("plan", "tables", 
    "name"), ds.omop.plan.person_period = c("plan", "table", 
    "concept_set", "bin_width", "window_start", "window_end", 
    "analyses", "grain", "time_origin", "name"), ds.omop.plan.preview = c("plan", 
    "symbol", "conns"), ds.omop.plan.save = c("plan", "file", 
    "format"), ds.omop.plan.survival = c("plan", "outcome_table", 
    "outcome_concepts", "tar", "event_order", "name", "outcomes", 
    "censoring", "format", "washout_days", "tie_policy", "transitions", 
    "initial_state", "state_hierarchy", "state_step"), ds.omop.plan.temporal_covariates = c("plan", 
    "table", "concept_set", "bin_width", "window_start", "window_end", 
    "analyses", "name"), ds.omop.plan.validate = c("plan", "symbol", 
    "conns"), ds.omop.prevalence = c("concept_id", "cohort", 
    "domain", "top_n", "tables", "plot", "symbol", "conns", "type"
    ), ds.omop.query.exec = c("query_id", "inputs", "mode", "symbol", 
    "conns"), ds.omop.query.get = c("query_id", "symbol", "conns"
    ), ds.omop.query.list = c("domain", "provider", "symbol", 
    "conns"), ds.omop.query.pool = c("results", "query_id", "sensitive_fields", 
    "pool_strategy", "policy", "symbol"), ds.omop.querylibrary.sticky.release = c("x", 
    "redesign", "datasources", "pool", "format", "type", "symbol"
    ), ds.omop.safe.cutpoints = c("table", "column", "concept_id", 
    "n_bins", "scope", "symbol", "conns", "execute", "type"), 
    ds.omop.safe.filter.measurement = c("concept_id", "min_value", 
    "max_value", "n_bins", "symbol", "conns"), ds.omop.safe.filter.value = c("table", 
    "column", "threshold", "direction", "concept_id", "n_bins", 
    "symbol", "conns"), ds.omop.select = c("x", "cols", "newobj", 
    "symbol", "conns"), ds.omop.server.version = "conns", ds.omop.snapshot = c("symbol", 
    "conns"), ds.omop.status = "symbol", ds.omop.table.stats = c("table", 
    "stats", "scope", "pooling_policy", "symbol", "conns", "execute", 
    "type"), ds.omop.tables = c("schema_category", "symbol", 
    "conns"), ds.omop.value.counts = c("table", "column", "top_n", 
    "concept_id", "cohort", "scope", "pooling_policy", "symbol", 
    "conns", "execute", "type"), ds.omop.value.histogram = c("table", 
    "value_col", "bins", "concept_id", "cohort_table", "window", 
    "cohort", "scope", "pooling_policy", "symbol", "conns", "execute", 
    "plot", "nbins", "xlab", "main", "col", "type"), ds.omop.value.quantiles = c("table", 
    "value_col", "probs", "concept_id", "cohort_table", "window", 
    "cohort", "rounding", "scope", "pooling_policy", "symbol", 
    "conns", "execute", "type"), ds.omop.vocab.classes = c("symbol", 
    "conns", "execute"), ds.omop.vocab.domains = c("symbol", 
    "conns", "execute"), ds.omop.vocab.vocabularies = c("symbol", 
    "conns", "execute"), omop_filter = c("type", "level", "params", 
    "label"), omop_filter_age = c("min", "max", "year", "reference_date"
    ), omop_filter_age_group = c("groups", "year", "reference_date"
    ), omop_filter_cohort = "cohort_definition_id", omop_filter_concept_count = c("concept_id", 
    "table", "min_count", "concept_name", "window", "reference_date"
    ), omop_filter_date_range = c("start", "end", "date_column"
    ), omop_filter_followup = c("min_days", "reference_date"), 
    omop_filter_group = c("...", "operator", "label"), omop_filter_has_concept = c("concept_id", 
    "table", "concept_name", "window", "min_count", "reference_date"
    ), omop_filter_has_measurement = c("concept_id", "min_value", 
    "max_value", "safe_bins", "window", "reference_date"), omop_filter_missing_measurement = c("concept_id", 
    "window", "reference_date"), omop_filter_not_has_concept = c("concept_id", 
    "table", "concept_name", "window", "reference_date"), omop_filter_prior_observation = c("min_days", 
    "reference_date"), omop_filter_sex = "value", omop_filter_value = c("column", 
    "threshold", "direction", "safe_bins"), omop_filter_value_concept = c("concept_ids", 
    "column", "concept_name"), omop_filter_visit_count = c("min_count", 
    "visit_concept_id", "window", "reference_date"), omop_index_event = c("concept_id", 
    "table", "concept_name", "primary_limit", "include_descendants", 
    "include_mapped", "end_strategy"), omop_output = c("name", 
    "type", "variables", "population_id", "options", "result_symbol"
    ), omop_population = c("id", "label", "parent_id", "filters", 
    "cohort_definition_id", "episode_policy", "union", "intersect", 
    "setdiff", "index_event"), omop_privacy = c("statistic", 
    "variable", "levels", "breaks", "lower", "upper", "reducer", 
    "max_contributions", "positive", "order_by", "denominator", 
    "population_id"), omop_querylibrary_sticky = c("upstream_id", 
    "variable", "levels", "breaks", "lower", "upper", "positive", 
    "max_contributions", "order_by", "population_id", "preparation"
    ), omop_querylibrary_sticky_catalog = "include_unavailable", 
    omop_recipe = c("variables", "filters", "outputs", "populations", 
    "blocks", "cohort", "tables", "combine", "options", "output"
    ), omop_variable = c("name", "table", "column", "concept_id", 
    "concept_name", "type", "format", "value_source", "time_window", 
    "suffix_mode", "filters", "visit_filter", "concept_col", 
    "expand", "reference_date", "unit"), omop_variable_age = c("name", 
    "reference", "reference_date", "year"), omop_variable_block = c("id", 
    "table", "concept_ids", "concept_names", "time_window", "format", 
    "value_source", "suffix_mode", "filters", "population_id", 
    "expand", "reference_date", "unit"), omop_variable_chads2 = c("name", 
    "reference_date"), omop_variable_chadsvasc = c("name", "reference_date"
    ), omop_variable_charlson = "name", omop_variable_cv = c("table", 
    "concept_id", "concept_name", "name", "value_source"), omop_variable_dcsi = "name", 
    omop_variable_demo_missingness = "name", omop_variable_drug_duration = c("concept_id", 
    "concept_name", "name", "agg"), omop_variable_followup = c("name", 
    "reference_date"), omop_variable_hfrs = "name", omop_variable_n_distinct = c("table", 
    "name"), omop_variable_obs_duration = c("name", "period_policy"
    ), omop_variable_prior_obs = c("name", "reference_date"), 
    omop_variable_sd = c("table", "concept_id", "concept_name", 
    "name", "value_source"), omop_variable_sex = "name", omop_variable_slope = c("table", 
    "concept_id", "concept_name", "name", "value_source"), omop_variable_sum = c("table", 
    "column", "concept_id", "concept_name", "name"), omop.date_handling = c("mode", 
    "reference", "bin_width", "date_columns"), omop.feature.abnormal_high = c("concept_set", 
    "name"), omop.feature.abnormal_low = c("concept_set", "name"
    ), omop.feature.boolean = c("concept_set", "name"), omop.feature.count = c("concept_set", 
    "name"), omop.feature.cv_value = c("concept_set", "value_column", 
    "name"), omop.feature.drug_duration = c("concept_set", "agg", 
    "name"), omop.feature.duration_sum = c("concept_set", "name"
    ), omop.feature.first_value = c("concept_set", "value_column", 
    "name"), omop.feature.gap_max_days = c("concept_set", "name"
    ), omop.feature.gap_mean_days = c("concept_set", "name"), 
    omop.feature.latest_value = c("concept_set", "value_column", 
    "name"), omop.feature.max_value = c("concept_set", "value_column", 
    "name"), omop.feature.mean_value = c("concept_set", "value_column", 
    "name"), omop.feature.min_value = c("concept_set", "value_column", 
    "name"), omop.feature.n_distinct = c("concept_set", "name"
    ), omop.feature.sd_value = c("concept_set", "value_column", 
    "name"), omop.feature.slope_value = c("concept_set", "value_column", 
    "name"), omop.feature.sum_value = c("concept_set", "value_column", 
    "name"), omop.feature.time_since = c("concept_set", "reference_date", 
    "unit", "name"), omop.temporal = c("index_window", "calendar", 
    "event_select", "min_gap"), recipe_execute = c("recipe", 
    "out", "symbol", "conns", "output_mode", "cohort", "tables", 
    "combine"), recipe_export_circe = c("recipe", "population_id", 
    "file"), recipe_export_json = c("recipe", "file"), recipe_export_yaml = c("recipe", 
    "file"), recipe_import_circe = c("file_or_json", "id", "label"
    ), recipe_import_json = "json", recipe_import_yaml = "yaml", 
    recipe_lint = "recipe", recipe_load = "file", recipe_preview = c("recipe", 
    "symbol", "conns"), recipe_preview_schema = "recipe", recipe_preview_stats = c("recipe", 
    "scope", "symbol", "conns"), recipe_save = c("recipe", "file", 
    "format"), recipe_to_code = "recipe", recipe_to_plan = "recipe", 
    recipe_validate = c("recipe", "symbol", "conns"))

test_that("every 2.10.1 export keeps its positional formals (new arguments appended)", {
  ns <- asNamespace("dsOMOPClient")
  exports <- getNamespaceExports("dsOMOPClient")
  for (nm in names(.formals_2101)) {
    expect_true(nm %in% exports, info = nm)
    now <- names(formals(get(nm, envir = ns)))
    old <- .formals_2101[[nm]]
    expect_identical(now[seq_along(old)], old, info = nm)
  }
  # zero-argument 2.10.1 exports stay exported
  expect_true(all(c("ds.omop.plan", "ds.omop.client.version") %in% exports))
})

test_that("the appended 2.10.3 arguments are the frozen ones", {
  appended <- function(f, n) utils::tail(names(formals(f)), n)
  expect_identical(appended(omop_variable, 5L),
                   c("event", "reducer", "where", "visit", "broadcast"))
  # F-24 keeps visit_filter as the legacy alias of visit (appended last)
  expect_identical(appended(omop_variable_block, 4L),
                   c("where", "visit", "concept_col", "visit_filter"))
  expect_identical(appended(omop_population, 3L), c("index", "match", "coverage"))
  expect_identical(appended(omop_output, 6L),
                   c("rows", "grain", "columns", "format", "sources", "collapse"))
  expect_identical(appended(omop_recipe, 5L),
                   c("concept_sets", "events", "intervals", "coverage", "publish"))
  expect_identical(appended(recipe_execute, 1L), "on_small_site")
  expect_identical(names(formals(recipe_to_plan)), c("recipe", "version"))
  expect_identical(formals(recipe_to_plan)$version, 2L)
  expect_null(formals(omop_output)$type)
  expect_null(formals(omop_variable)$table)
  expect_identical(names(formals(ds.omop.filter))[1:4], c("x", "var", "op", "value"))
})

test_that("positional 2.10.1 calls keep their meaning", {
  v <- omop_variable("hba1c", "measurement", "value_as_number", 3004410, "HbA1c",
                     "numeric", "mean")
  expect_identical(v$table, "measurement")
  expect_identical(v$column, "value_as_number")
  expect_identical(v$concept_id, 3004410L)
  expect_identical(v$format, "mean")
  p <- omop_population("f", "Females", "base", list(omop_filter_sex("F")), 7L)
  expect_identical(p$parent_id, "base")
  expect_identical(p$cohort_definition_id, 7L)
  o <- omop_output("o", "long", c("a", "b"), "f", list(), "D_x")
  expect_identical(o$type, "long")
  expect_identical(o$variables, c("a", "b"))
  expect_identical(o$population_id, "f")
  expect_identical(o$result_symbol, "D_x")
  b <- omop_variable_block("blk", "condition_occurrence", c(1, 2), c("a", "b"),
                           list(start = -30, end = 0), "binary")
  expect_identical(b$format, "binary")
  expect_identical(b$time_window, list(start = -30, end = 0))
  r <- omop_recipe(list(omop_variable_sex()), NULL, list(omop_output("w")))
  expect_identical(names(r$outputs), "w")
  expect_identical(omop_index_event(201820, "condition_occurrence", NULL, "last")$primary_limit,
                   "last")
  expect_identical(omop_filter_age(18, 65)$params$min, 18)
  expect_identical(omop_filter_has_concept(201820, "condition_occurrence")$params$table,
                   "condition_occurrence")
})
