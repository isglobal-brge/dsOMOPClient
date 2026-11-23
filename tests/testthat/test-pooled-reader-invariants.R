# Every exported reader with a type view belongs to this manifest. The thirteen
# basic aggregate readers are exercised by test-pooled-frames.R; this
# file runs the remaining public wrappers with three-site released fixtures.
.pi_core_readers <- paste0('ds.omop.', c(
  'table.stats', 'column.stats', 'domain.coverage', 'missingness', 'value.counts',
  'crosstab', 'concept.prevalence', 'value.histogram', 'date.counts',
  'concept.drilldown', 'concept.locate', 'achilles.results',
  'achilles.distribution'))
.pi_extra_readers <- c(paste0('ds.omop.', c(
  'tables', 'columns', 'disclosure.settings', 'concept.search', 'concept.lookup',
  'concept.descendants', 'concept.expand', 'concept.ancestors', 'concept.synonyms',
  'concept.relationships', 'concept.list', 'vocab.vocabularies', 'vocab.domains',
  'vocab.classes', 'cdm.version', 'achilles.analyses', 'achilles.catalog',
  'analysis.list', 'analysis.get', 'analysis.run', 'prevalence', 'distribution',
  'meta.effect_estimate', 'ohdsi.tables', 'ohdsi.results', 'dp.release',
  'querylibrary.sticky.release')), 'recipe_preview_stats')
.pi_unpoolable_readers <- c(paste0('ds.omop.', c(
  'safe.cutpoints', 'value.quantiles', 'concept.summary', 'cohort.list',
  'achilles.status', 'ohdsi.status', 'ohdsi.summary', 'dp.status', 'cdm.source',
  'plan.validate', 'plan.preview')), 'recipe_validate', 'recipe_preview')
.pi_legacy_readers <- paste0('ds.omop.query.', c('list', 'get', 'pool'))
.pi_sites <- c('site_a', 'site_b', 'site_c')
.pi_conns <- setNames(as.list(seq_len(3L)), .pi_sites)
.pi_replicate <- function(value) setNames(rep(list(value), 3L), .pi_sites)
.pi_no_null <- function(value) {
  !is.null(value) && (!is.list(value) || all(vapply(value, .pi_no_null, logical(1))))
}
.pi_check <- function(value, type, label) {
  expect_true(is.data.frame(as.data.frame(value)), info = label)
  expect_true(length(capture.output(print(value))) > 0L, info = label)
  if (type == 'split') {
    expect_null(value$pooled, info = label)
    expect_true(setequal(unique(as.data.frame(value)$server), .pi_sites), info = label)
  } else {
    expect_true(.pi_no_null(value$pooled), info = label)
    expect_true(is.data.frame(as.data.frame(value$pooled)), info = label)
  }
}
.pi_session <- function(symbol) list(conns = .pi_conns, res_symbol = 'omop_obj',
  server_versions = setNames(rep('2.10.10', 3L), .pi_sites))
.pi_settings <- function() {
  out <- list(harmonization_contract_version = 'dsomop-harmonization-v3',
    age_semantics = 'completed_years', date_semantics = 'calendar',
    date_granularity = 'month', datetime_timezone = 'UTC', week_start = 'Monday',
    age_breaks = c(0L, 5L, 10L), nfilter_band = 5, nfilter_age_range = 5,
    nfilter_date_range = 30)
  for (field in c('max_feature_specs', 'max_pivot_concepts', 'max_output_columns',
    'max_temporal_bins', 'max_events_per_group', 'max_filter_depth',
    'max_filter_nodes', 'max_filter_values', 'max_plan_outputs',
    'max_analysis_scope_tables')) out[[field]] <- 100L
  out
}

test_that('the reader invariant manifest cannot silently omit a new view', {
  exports <- getNamespaceExports('dsOMOPClient')
  views <- Filter(function(name) {
    fn <- get(name, asNamespace('dsOMOPClient'))
    if (!is.function(fn) || !'type' %in% names(formals(fn))) return(FALSE)
    value <- deparse(formals(fn)[['type']])
    length(value) == 1L && value %in% c('"split"', '"combine"', '"both"')
  }, exports)
  expect_setequal(views, c(.pi_core_readers, .pi_extra_readers,
                          .pi_unpoolable_readers))
  legacy_views <- Filter(function(name) {
    fn <- get(name, asNamespace('dsOMOPClient'))
    if (!is.function(fn) || 'type' %in% names(formals(fn))) return(FALSE)
    body_text <- paste(deparse(body(fn)), collapse = ' ')
    any(vapply(c('pooled', '.pool_', '.reader_result', '.as_listing'),
      grepl, logical(1), x = body_text, fixed = TRUE))
  }, exports)
  expect_setequal(legacy_views, .pi_legacy_readers)
})

test_that('metadata and catalog readers preserve shape for every view', {
  frame <- data.frame(concept_id = 1L, concept_name = 'Reference',
                       valid_start_date = as.Date('2020-01-01'))
  cases <- list(
    'ds.omop.tables' = list(args = list(), raw = data.frame(table_name = 'person',
      has_person_id = TRUE)),
    'ds.omop.columns' = list(args = list(table = 'person'), raw = data.frame(
      column_name = 'gender_concept_id', is_concept = TRUE)),
    'ds.omop.disclosure.settings' = list(args = list(), raw = .pi_settings()),
    'ds.omop.concept.search' = list(args = list(pattern = 'Reference'), raw = frame),
    'ds.omop.concept.lookup' = list(args = list(concept_ids = 1L), raw = frame),
    'ds.omop.concept.descendants' = list(args = list(ancestor_ids = 1L), raw = frame),
    'ds.omop.concept.expand' = list(args = list(concept_set = ds.omop.concept.set(1L)), raw = frame),
    'ds.omop.concept.ancestors' = list(args = list(concept_ids = 1L), raw = frame),
    'ds.omop.concept.synonyms' = list(args = list(concept_ids = 1L), raw = frame),
    'ds.omop.concept.relationships' = list(args = list(concept_ids = 1L), raw = frame),
    'ds.omop.concept.list' = list(args = list(), raw = list(rows = frame,
      total_count = 1L, offset = 0L, limit = 100L)),
    'ds.omop.vocab.vocabularies' = list(args = list(), raw = frame),
    'ds.omop.vocab.domains' = list(args = list(), raw = frame),
    'ds.omop.vocab.classes' = list(args = list(), raw = frame),
    'ds.omop.cdm.version' = list(args = list(), raw = list(cdm_version = '5.4')),
    'ds.omop.achilles.analyses' = list(args = list(), raw = data.frame(analysis_id = 1L)),
    'ds.omop.achilles.catalog' = list(args = list(), raw = data.frame(analysis_id = 1L)),
    'ds.omop.analysis.list' = list(args = list(), raw = data.frame(name = 'test:count', mode = 'aggregate')),
    'ds.omop.analysis.get' = list(args = list(name = 'test:count'), raw = list(
      name = 'test:count', mode = 'aggregate', plot = NULL, params = list())),
    'ds.omop.ohdsi.tables' = list(args = list(), raw = data.frame(
      table_name = 'cohort_count', n_rows = 10L))
  )
  raw <- NULL
  testthat::local_mocked_bindings(.get_session = .pi_session,
    .ds_safe_aggregate = function(conns, expr) raw, .package = 'dsOMOPClient')
  for (name in names(cases)) {
    raw <- .pi_replicate(cases[[name]]$raw)
    for (type in c('split', 'combine', 'both')) {
      result <- do.call(get(name), c(cases[[name]]$args, list(type = type)))
      .pi_check(result, type, paste(name, type))
    }
  }
})

.pi_contract <- function() list(version = 1L, strategy = 'tabular', columns = list(
  group = list(role = 'key'), n = list(role = 'sum'), n_persons = list(role = 'sum'),
  mean = list(role = 'weighted_mean', weight = 'n'),
  median = list(role = 'nonpoolable', reason = 'No released union distribution.')))
.pi_analysis_meta <- function(contract = .pi_contract()) list(name = 'test:summary',
  mode = 'aggregate', pooling_contract = contract, release_pooling = NULL)

test_that('deprecated query views preserve the catalog and contractual pool', {
  raw <- setNames(lapply(seq_len(3L), function(i) data.frame(group = 'all',
    n = i * 10, n_persons = i * 5, mean = c(2, 4, 8)[i], median = i)), .pi_sites)
  meta <- .pi_analysis_meta()
  meta$plot <- NULL
  testthat::local_mocked_bindings(.get_session = .pi_session,
    .session_harmonization_for_connections = function(...) NULL,
    .ds_safe_aggregate = function(conns, expr) {
      switch(as.character(expr[[1L]]),
        omopAnalysisListDS = .pi_replicate(data.frame(name = 'test:summary',
          mode = 'aggregate')),
        omopAnalysisGetDS = .pi_replicate(meta),
        omopAnalysisRunDS = raw,
        stop('Unexpected aggregate fixture'))
    }, .package = 'dsOMOPClient')
  listing <- suppressWarnings(ds.omop.query.list())
  details <- suppressWarnings(ds.omop.query.get('test:summary'))
  released <- suppressWarnings(ds.omop.query.exec('test:summary'))
  pooled <- suppressWarnings(ds.omop.query.pool(released, 'test:summary'))
  for (value in list(listing, details, pooled)) {
    expect_true(.pi_no_null(value))
    expect_true(is.data.frame(as.data.frame(value)))
    expect_true(length(capture.output(print(value))) > 0L)
  }
  expect_equal(pooled$n, 60)
  expect_equal(pooled$n_persons, 30)
  expect_equal(pooled$mean, 17 / 3)
  expect_true(is.na(pooled$median))
})

test_that('analysis wrappers use real contracted algebra and retain counts', {
  raw <- setNames(lapply(seq_len(3L), function(i) data.frame(group = 'all',
    n = i * 10, n_persons = i * 5, mean = c(2, 4, 8)[i], median = i)), .pi_sites)
  meta <- .pi_analysis_meta()
  testthat::local_mocked_bindings(.get_session = .pi_session,
    .session_harmonization_for_connections = function(...) NULL,
    .ds_safe_aggregate = function(conns, expr) {
      switch(as.character(expr[[1L]]),
        omopAnalysisGetDS = .pi_replicate(meta),
        omopAnalysisRunDS = raw,
        omopOhdsiResultContractDS = .pi_replicate(list(contract_version = 1L,
          tool_id = 'cohort_diagnostics', table_name = 'cohort_count',
          pooling_contract = meta$pooling_contract)),
        omopOhdsiResultsDS = raw,
        stop('Unexpected aggregate fixture'))
    }, .package = 'dsOMOPClient')
  cases <- list(
    'ds.omop.analysis.run' = list(name = 'test:summary'),
    'ds.omop.prevalence' = list(cohort = 1L),
    'ds.omop.distribution' = list(cohort = 1L),
    'ds.omop.meta.effect_estimate' = list(name = 'test:summary'),
    'ds.omop.ohdsi.results' = list(table_name = 'cohort_count'))
  for (name in names(cases)) for (type in c('split', 'combine', 'both')) {
    result <- do.call(get(name), c(cases[[name]], list(type = type)))
    .pi_check(result, type, paste(name, type))
    if (type != 'split') {
      expect_identical(result$pooled$n, 60)
      expect_identical(result$pooled$n_persons, 30)
      expect_equal(result$pooled$mean, 17 / 3)
      expect_true(is.na(result$pooled$median))
      expect_match(paste(result$meta$warnings, collapse = ' '), 'Non-poolable')
    }
  }
  # The effect alias delegates to the server's inverse-variance contract.
  contract <- list(version = 1L, strategy = 'effect_estimate', columns = list(
    n_persons = list(role = 'sum'),
    log_rr = list(role = 'nonpoolable', reason = 'Meta-analysis required.'),
    se = list(role = 'nonpoolable', reason = 'Meta-analysis required.')),
    log_estimate = 'log_rr', standard_error = 'se', transform = 'exp',
    strata = character())
  meta <- .pi_analysis_meta(contract)
  raw <- setNames(lapply(seq_len(3L), function(i) data.frame(n_persons = i * 10,
    log_rr = log(c(1, 2, 4)[i]), se = 0.2)), .pi_sites)
  for (type in c('split', 'combine', 'both')) {
    result <- ds.omop.meta.effect_estimate('test:summary', type = type)
    .pi_check(result, type, paste('effect contract', type))
    if (type != 'split') {
      expect_equal(result$pooled$estimate_fixed, 2)
      expect_equal(result$pooled$n_persons, 60)
      expect_true(is.na(result$pooled$log_rr))
    }
  }

  # Protected analysis displays sum signed components before projection.
  meta <- .pi_analysis_meta()
  meta$release_pooling <- list(strategy = 'protected_components')
  raw <- setNames(lapply(seq_len(3L), function(i) {
    frame <- data.frame(cell = c('A', 'B'), n_persons = c(7, 3) * i)
    attr(frame, 'dsomop_release') <- list(
      protocol = .DP_PROTECTED_RELEASE_PROTOCOL, entry = 'test:summary',
      protection = 'realised_sticky',
      source = list(logical_source_id = paste0('dsl_', strrep(as.character(i), 40))),
      owners = list(list(owner = 'histogram/v1', family = 'H', grid = 'public',
        bounds = NULL, strata_kind = 'root', cell_keys = c('A', 'B'),
        allocation = list(N = 0.2, H = 0.8),
        components = list(list(stratum = 'root', n = i * 10, h = c(7, 3) * i)))))
    frame
  }), .pi_sites)
  for (type in c('split', 'combine', 'both')) {
    result <- ds.omop.analysis.run('test:summary', type = type)
    .pi_check(result, type, paste('protected catalog', type))
    if (type != 'split') {
      expect_equal(result$pooled$persons, 60)
      expect_equal(attr(result$pooled, 'cells')$count, c(42, 18))
      expect_true('n_persons' %in% names(result$pooled))
      expect_true(all(is.na(result$pooled$n_persons)))
      expect_match(paste(result$meta$warnings, collapse = ' '), 'no pooled statistic')
    }
  }
})

.pi_dp_payload <- function(i, statistic = 'count') {
  n <- c(12, -2, 20)[i]
  list(protocol = .DP_PROTECTED_RELEASE_PROTOCOL, statistic = statistic,
    protection = 'realised_sticky',
    guarantee_scope = 'heuristic_realised_contribution_reuse',
    mechanism_epsilon = NA_real_, noise_reference_epsilon = 0.1,
    api_transcript_epsilon = NA_real_, privacy_call_quota = 'none',
    history_based_admission = FALSE, noisy_count = max(0, n),
    source = list(logical_source_id = paste0('dsl_', strrep(as.character(i), 40))),
    components = list(owner = 'typed/count/v1', family = 'N',
      coordinates = 'stratum=root', families = 'N', signed = n,
      projection = 'none', view_kind = 'basis', basis = list(
        id = 'typed-count-basis/v1', coordinates = 'stratum=root',
        families = 'N', signed = n)))
}
.pi_dp_statuses <- function() setNames(lapply(seq_len(3L), function(i) list(
  enabled = TRUE, ready = TRUE, privacy_contract = .DP_PROTECTED_PRIVACY_CONTRACT,
  privacy_guarantee = .DP_PROTECTED_GUARANTEE, history_based_admission = FALSE,
  privacy_call_quota = 'none', supported_statistics = c('count', 'bounded_distinct'),
  noise_domain_id = paste0('dpn_', strrep(as.character(i), 40)))), .pi_sites)

test_that('DP and sticky wrappers convert derived displays without wire-schema fields', {
  privacy <- omop_privacy('count')
  raw <- setNames(lapply(seq_len(3L), .pi_dp_payload), .pi_sites)
  testthat::local_mocked_bindings(.dp_statuses = function(datasources) .pi_dp_statuses(),
    .ds_safe_aggregate = function(conns, expr) raw,
    .querylibrary_sticky_validate_redesign = function(...) invisible(NULL),
    .querylibrary_sticky_server_preflight = function(...) invisible(NULL),
    .package = 'dsOMOPClient')
  cases <- list(
    'ds.omop.dp.release' = list(x = 'D', privacy = privacy),
    'ds.omop.querylibrary.sticky.release' = list(x = 'D', redesign = list(privacy = privacy)))
  for (name in names(cases)) for (type in c('split', 'combine', 'both')) {
    result <- do.call(get(name), c(cases[[name]], list(type = type, datasources = .pi_conns)))
    .pi_check(result, type, paste(name, type))
    if (type != 'split') {
      expect_equal(result$pooled$noisy_count, 30)
      expect_false('components' %in% names(result$pooled))
      expect_false('protocol' %in% names(result$pooled))
    }
  }
  raw <- setNames(lapply(seq_len(3L), .pi_dp_payload,
    statistic = 'bounded_distinct'), .pi_sites)
  distinct <- omop_privacy('bounded_distinct', variable = 'code',
    levels = c('A', 'B'), max_contributions = 2)
  both <- ds.omop.dp.release('D', distinct, datasources = .pi_conns, type = 'both')
  expect_null(both$pooled)
  expect_match(both$meta$pooling_reason, 'not additive')
  expect_error(ds.omop.dp.release('D', distinct, datasources = .pi_conns,
    type = 'combine'), 'not additive')
})

test_that('recipe statistics retain complete per-table and per-site count views', {
  raw <- setNames(lapply(seq_len(3L), function(i) list(rows = i * 20,
    persons = i * 10)), .pi_sites)
  testthat::local_mocked_bindings(.get_session = .pi_session,
    .ds_safe_aggregate = function(conns, expr) raw,
    .pooling_harmonization_for_servers = function(...) NULL,
    .package = 'dsOMOPClient')
  recipe <- structure(list(variables = list(list(table = 'person'))), class = 'omop_recipe')
  for (type in c('split', 'combine', 'both')) {
    result <- recipe_preview_stats(recipe, type = type)
    .pi_check(result, type, paste('recipe_preview_stats', type))
    if (type != 'split') {
      expect_equal(result$pooled$rows, 120)
      expect_equal(result$pooled$persons, 60)
    }
  }
})

test_that('legacy DP histogram formats use the shared conversion boundary', {
  privacy <- list(statistic = 'categorical_histogram', levels = c('A', 'B'))
  raw <- setNames(lapply(seq_len(3L), function(i) list(counts = c(i, 2 * i))), .pi_sites)
  for (format in c('long', 'wide', 'vector', 'raw')) {
    pooled <- .dp_pool_release(raw, privacy, list(), format)
    for (type in c('split', 'combine', 'both')) {
      result <- .result_type_view(dsomop_result(raw, pooled, schema = list()), type)
      .pi_check(result, type, paste('legacy DP', format, type))
    }
  }
})

test_that('site-specific readers convert split results and explain pooling refusal', {
  raw <- .pi_replicate(list(available = TRUE, detail = NULL))
  testthat::local_mocked_bindings(.get_session = .pi_session,
    .ds_safe_aggregate = function(conns, expr) raw,
    .dp_statuses = function(datasources) raw,
    .session_harmonization_for_connections = function(...) NULL,
    .prepare_plan_for_federation = function(plan, ...) plan,
    .recipe_plan_for_session = function(...) ds.omop.plan(),
    .package = 'dsOMOPClient')
  cases <- list(
    'ds.omop.safe.cutpoints' = list(table = 'measurement', column = 'value_as_number'),
    'ds.omop.value.quantiles' = list(table = 'measurement', value_col = 'value_as_number'),
    'ds.omop.cohort.list' = list(), 'ds.omop.achilles.status' = list(),
    'ds.omop.ohdsi.status' = list(), 'ds.omop.ohdsi.summary' = list(tool_id = 'cohort_diagnostics'),
    'ds.omop.dp.status' = list(datasources = .pi_conns), 'ds.omop.cdm.source' = list(),
    'ds.omop.plan.validate' = list(plan = ds.omop.plan()),
    'ds.omop.plan.preview' = list(plan = ds.omop.plan()),
    'recipe_validate' = list(recipe = structure(list(), class = 'omop_recipe')),
    'recipe_preview' = list(recipe = structure(list(), class = 'omop_recipe')))
  for (name in names(cases)) {
    for (type in c('split', 'both')) {
      result <- do.call(get(name), c(cases[[name]], list(type = type)))
      expect_true(is.data.frame(as.data.frame(result)), info = name)
      expect_true(length(capture.output(print(result))) > 0L, info = name)
      expect_null(result$pooled, info = name)
      if (type == 'split') {
        expect_true(setequal(unique(as.data.frame(result)$server), .pi_sites), info = name)
      } else expect_true(nzchar(result$meta$pooling_reason), info = name)
    }
    expect_error(do.call(get(name), c(cases[[name]], list(type = 'combine'))),
      'cannot|never|per server|site-specific', info = name)
  }
  # concept.summary is a convenience composite, tested with actual child result
  # objects in test-pooled-metadata-shape.R; its combine refusal remains explicit.
})
