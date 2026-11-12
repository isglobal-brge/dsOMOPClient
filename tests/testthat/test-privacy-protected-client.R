# dsOMOPClient 2.10.3: protected releases of dsOMOP 2.10.3 servers. Payloads
# are validated (protocol, route, guarantee scope, no private material, no
# history dependence), pooled by summing compatible signed components and
# deriving each view once, never mixed with per-release (2.10.2 or earlier)
# servers, and labelled without relabelling heuristic releases as DP.

.pc_status <- function(...) {
  utils::modifyList(list(
    enabled = TRUE, ready = TRUE, sticky_noise = TRUE, exclusive = FALSE,
    protocol = .DP_PROTOCOL,
    canonical_protocol = "dsomop-dp-canonical-json-v1",
    mechanism = "dsomop-sticky-discrete-laplace-prf-v2",
    sampler = "dsomop-dp-exact-discrete-laplace-v1",
    privacy_guarantee = .DP_PROTECTED_GUARANTEE,
    privacy_contract = .DP_PROTECTED_PRIVACY_CONTRACT,
    release_protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
    person_local_provenance_required = TRUE,
    provenance_protocol = "dsomop-dp-person-local-provenance-v2",
    adjacency = "add_remove_person", domain = "dsomop-dp-test",
    snapshot_id = "snapshot-2026-11-12", release_epsilon = 0.1,
    release_delta = 0,
    supported_statistics = c(
      "count", "bounded_record_count", "categorical_histogram",
      "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate"),
    longitudinal_contract = "deterministic_person_bounding_v1",
    privacy_epoch = 1L, max_levels = 1000L, max_contributions = 10L,
    numeric_grid = 65535L, history_dependent = FALSE,
    history_based_admission = FALSE, privacy_call_quota = "none",
    persistent_state = "noise_root_only",
    release_binding = .DP_PROTECTED_BINDING, service_capacity = "none"
  ), list(...))
}

# A public logical source identifier (one per logical node).
.pc_source <- function(i) {
  list(logical_source_id = paste0("dsl_", strrep(as.character(i), 40L)))
}

# One protected payload as dsOMOP 2.10.3 returns it: only a fixed synopsis
# declares a delta (0); heuristic releases carry none.
.pc_payload <- function(statistic, signed, coordinates, families, shown,
                        protection = "realised_sticky", ...) {
  fixed <- identical(protection, "fixed_synopsis")
  payload <- utils::modifyList(c(list(
    protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
    release_contract = "dsomop-release-contract-v1",
    statistic = statistic, protection = protection,
    guarantee_scope = if (fixed) "fixed_vector_before_exact_support_gate" else
      "heuristic_realised_contribution_reuse",
    noise_reference_epsilon = 0.1,
    mechanism_epsilon = if (fixed) 0.1 else NA_real_,
    api_transcript_epsilon = NA_real_,
    adjacency = "add_remove_person",
    mechanism = "dsomop-sticky-discrete-laplace-prf-v2",
    sampler = "dsomop-dp-exact-discrete-laplace-v1", sticky = TRUE,
    allocation = list(N = 0.02), sensitivity = list(N = 1),
    noise_sd = list(N = 70.7), privacy_call_quota = "none",
    history_based_admission = FALSE), shown,
    list(components = list(owner = paste0("typed/", statistic, "/v1"),
                           family = families[[length(families)]],
                           coordinates = coordinates, families = families,
                           reconciliation =
                             "declared_cells_sum_to_person_total",
                           signed = signed))), list(...))
  if (fixed && !"delta" %in% names(list(...))) payload$delta <- 0
  payload
}

# Mock servers: every status gets its own noise domain and every release that
# names no logical source gets a distinct one, unless fill_source = FALSE.
.pc_backend <- function(statuses, releases, code, fill_source = TRUE) {
  for (i in seq_along(statuses)) {
    statuses[[i]]$noise_domain_id <- statuses[[i]]$noise_domain_id %||%
      paste0("dpn_", strrep(as.character(i), 40L))
  }
  if (isTRUE(fill_source)) {
    for (i in seq_along(releases)) {
      if (is.list(releases[[i]]) && is.null(releases[[i]]$source)) {
        releases[[i]]$source <- .pc_source(i)
      }
    }
  }
  datasources <- stats::setNames(as.list(paste0("FAKE_", seq_along(statuses))),
                                 names(statuses))
  sent <- new.env(parent = emptyenv())
  sent$heads <- character(0)
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      head <- if (is.call(expr)) as.character(expr[[1L]]) else ""
      sent$heads <- c(sent$heads, head)
      value <- switch(head,
        omopDpStatusDS = statuses[[server]],
        omopDpReleaseDS = releases[[server]],
        stop("unexpected method ", head, call. = FALSE))
      stats::setNames(list(value), server)
    },
    .package = "DSI", .env = parent.frame())
  code(datasources, sent)
}

test_that("protected counts pool their signed person totals", {
  statuses <- list(a = .pc_status(), b = .pc_status())
  releases <- list(
    a = .pc_payload("count", 103.4, "stratum=root", "N",
                    list(noisy_count = 103.4)),
    b = .pc_payload("count", -2.5, "stratum=root", "N",
                    list(noisy_count = 0)))
  .pc_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                datasources)
    # Pooling sums the signed components (not the clipped per-site 0).
    expect_equal(value$pooled$noisy_count, 100.9)
    expect_identical(value$pooled$pooling,
                     "sum_of_signed_basis_components_then_derived_once")
    expect_identical(value$pooled$protection, "realised_sticky")
    privacy <- value$meta$privacy
    expect_identical(privacy$contract, .DP_PROTECTED_PRIVACY_CONTRACT)
    expect_identical(unname(privacy$protection),
                     c("realised_sticky", "realised_sticky"))
    expect_true(is.na(privacy$pooled_epsilon_disjoint_sites))
    expect_true(is.na(privacy$api_transcript_epsilon))
    expect_false(privacy$history_based_admission)
    expect_match(paste(value$meta$warnings, collapse = " "),
                 "not a joint differential-privacy guarantee")
    expect_setequal(unique(sent$heads), c("omopDpStatusDS", "omopDpReleaseDS"))
  })
})

test_that("fixed synopses report their declared vector epsilon", {
  statuses <- list(a = .pc_status(), b = .pc_status())
  releases <- list(
    a = .pc_payload("count", 50, "stratum=root", "N", list(noisy_count = 50),
                    protection = "fixed_synopsis", mechanism_epsilon = 0.02),
    b = .pc_payload("count", 70, "stratum=root", "N", list(noisy_count = 70),
                    protection = "fixed_synopsis", mechanism_epsilon = 0.02))
  .pc_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                datasources)
    expect_equal(value$pooled$noisy_count, 120)
    expect_identical(value$pooled$protection, "fixed_synopsis")
    expect_equal(value$meta$privacy$pooled_epsilon_disjoint_sites, 0.02)
    expect_equal(value$meta$privacy$pooled_epsilon_overlapping_sites, 0.04)
    expect_length(value$meta$warnings, 0L)
  })
})

test_that("protected histograms project pooled signed cells onto the pooled total", {
  statuses <- list(a = .pc_status(), b = .pc_status())
  coords <- c("stratum=root", "stratum=root|cell=F", "stratum=root|cell=M")
  releases <- list(
    a = .pc_payload("categorical_histogram", c(20, 17.5, 2.5), coords,
                    c("N", "H", "H"),
                    list(levels = c("F", "M"), counts = c(16, 4))),
    b = .pc_payload("categorical_histogram", c(30, 7.5, 22.5), coords,
                    c("N", "H", "H"),
                    list(levels = c("F", "M"), counts = c(7.5, 22.5))))
  .pc_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table",
      omop_privacy("categorical_histogram", "sex", levels = c("F", "M")),
      datasources)
    # Reconciled cells sum to each site's N; pooled N = 50 and the summed
    # cells (25, 25) project onto it unchanged.
    expect_equal(value$pooled$counts, c(25, 25))
    expect_identical(value$pooled$levels, c("F", "M"))
  })
})

test_that("protected bounded means pool their normalized moments", {
  statuses <- list(a = .pc_status(), b = .pc_status())
  J <- 65535
  coords <- c("stratum=root", "stratum=root", "stratum=root")
  # Site a: 10 people at z = 0.5; site b: 10 people at z = -0.5 (exact).
  releases <- list(
    a = .pc_payload("bounded_mean", c(10, 5 * J, 2.5 * J), coords,
                    c("N", "S", "Q"),
                    list(noisy_count = 10, value = 75, lower = 0,
                         upper = 100, scale = "count_per_person")),
    b = .pc_payload("bounded_mean", c(10, -5 * J, 2.5 * J), coords,
                    c("N", "S", "Q"),
                    list(noisy_count = 10, value = 25, lower = 0,
                         upper = 100, scale = "count_per_person")))
  .pc_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table",
      omop_privacy("bounded_mean", "x", lower = 0, upper = 100),
      datasources)
    expect_equal(value$pooled$noisy_count, 20)
    expect_equal(value$pooled$estimate, 50)
    expect_equal(value$pooled$sd, sqrt(5 / 19) * 50, tolerance = 1e-9)
  })
})

test_that("distinct-label cardinalities are never pooled across sites", {
  statuses <- list(a = .pc_status(), b = .pc_status())
  releases <- list(
    a = .pc_payload("bounded_distinct", 3.2, "distinct", "D",
                    list(noisy_count = 3.2)),
    b = .pc_payload("bounded_distinct", 2.1, "distinct", "D",
                    list(noisy_count = 2.1)))
  .pc_backend(statuses, releases, function(datasources, sent) {
    privacy <- omop_privacy("bounded_distinct", "concept",
                            levels = c("a", "b", "c"))
    expect_error(ds.omop.dp.release("analysis_table", privacy, datasources),
                 "not additive across sites")
    value <- ds.omop.dp.release("analysis_table", privacy, datasources,
                                pool = FALSE)
    expect_equal(value$per_site$a$noisy_count, 3.2)
  })
})

test_that("protected and per-release servers are never combined", {
  legacy <- .pc_status(privacy_contract = "fixed_per_release_seed_data_versioned_v1",
                       release_binding = "seed_data_versioned_v1",
                       privacy_guarantee = .DP_PRIVACY_GUARANTEE)
  legacy$history_based_admission <- NULL
  statuses <- list(a = .pc_status(), b = legacy)
  .pc_backend(statuses, list(), function(datasources, sent) {
    expect_error(ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                    datasources),
                 "cannot be combined")
    expect_false("omopDpReleaseDS" %in% sent$heads)
  })
})

test_that("protected payloads are validated before any pooling", {
  statuses <- list(a = .pc_status())
  base <- .pc_payload("count", 10, "stratum=root", "N", list(noisy_count = 10))
  bad <- list(
    claimed_epsilon = utils::modifyList(base, list(mechanism_epsilon = 0.1)),
    history = utils::modifyList(base, list(history_based_admission = TRUE)),
    quota = utils::modifyList(base, list(privacy_call_quota = "100")),
    scope = utils::modifyList(base, list(
      guarantee_scope = "fixed_vector_before_exact_support_gate")),
    protocol = utils::modifyList(base, list(protocol = "dsomop-dp-release-v3")),
    route = utils::modifyList(base, list(protection = "dp")),
    private = c(base, list(fingerprint = strrep("ab", 32L))),
    components = utils::modifyList(base, list(components = list(
      coordinates = "stratum=root", signed = NA_real_)))
  )
  for (name in names(bad)) {
    .pc_backend(statuses, list(a = bad[[name]]), function(datasources, sent) {
      expect_error(ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                      datasources), "Server 'a'",
                   info = name)
    })
  }
  # An incoherent protected status is refused before any release.
  status <- .pc_status(history_based_admission = TRUE)
  .pc_backend(list(a = status), list(a = base), function(datasources, sent) {
    expect_error(ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                    datasources))
    expect_false("omopDpReleaseDS" %in% sent$heads)
  })
})

# One protected catalogue frame as dsOMOP 2.10.3 returns it.
.pc_frame <- function(n, h, protection = "realised_sticky", source = 1L) {
  df <- data.frame(cell = c("0-4", "5-9"), n_persons = pmax(0, h))
  attr(df, "dsomop_release") <- list(
    protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
    contract = "dsomop-release-contract-v1",
    entry = "dsomop:achilles.101", protection = protection,
    reference_epsilon = 0.1,
    source = if (is.null(source)) NULL else .pc_source(source),
    owners = list(list(
      protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
      owner = "achilles/first-op-age-years/by-root/v1",
      population = "achilles/first-op-age-years/by-root/v1",
      family = "H", grid = "age_5y", breaks = c(0, 5, 10),
      bounds = NULL, strata_kind = "root",
      cell_keys = c("[0,5)", "[5,10)"), cells = NULL, lattice = NULL,
      cap = 1L, allocation = list(N = 0.2, H = 0.8, S = NULL, Q = NULL),
      reconciliation = "histogram_cells_sum_to_stratum_N",
      components = list(list(stratum = "root", n = n, s = NULL, q = NULL,
                             h = h)))))
  df
}

test_that("protected catalogue frames pool their signed components", {
  sources <- c(a = 1L, b = 2L)
  frame <- function(n, h, protection = "realised_sticky", site = "a") {
    .pc_frame(n, h, protection, source = sources[[site]])
  }
  pooled <- .pool_protected_release_frames(list(
    a = frame(10, c(7, 3)), b = frame(20, c(-5, 25), site = "b")))
  expect_equal(pooled$result$persons, 30)
  cells <- attr(pooled$result, "cells")
  # Summed (2, 28) projected onto 30.
  expect_equal(cells$count, c(2, 28))
  expect_identical(cells$cell, c("[0,5)", "[5,10)"))
  expect_match(paste(pooled$warnings, collapse = " "),
               "heuristic realised-contribution")
  # Standard and protected frames are never mixed.
  plain <- data.frame(cell = c("0-4", "5-9"), n_persons = c(5, 5))
  expect_error(.pool_protected_release_frames(list(a = frame(10, c(7, 3)),
                                                   b = plain)),
               "cannot be pooled together")
  # Incompatible grids are refused.
  other <- frame(20, c(5, 15), site = "b")
  release <- attr(other, "dsomop_release")
  release$owners[[1L]]$grid <- "age_10y"
  attr(other, "dsomop_release") <- release
  expect_error(.pool_protected_release_frames(list(a = frame(10, c(7, 3)),
                                                   b = other)),
               "not compatible")
})

test_that("protection labels print the protected routes and masked fields", {
  result <- dsomop_result(per_site = list(a = data.frame(x = 1)))
  label <- function(...) {
    result[["meta"]]$protection <- list(a = list(...))
    paste(capture.output(print(result)), collapse = "\n")
  }
  expect_match(label(protection = "realised_sticky",
                     noise_reference_epsilon = 0.1),
               "heuristic realised-contribution protection")
  expect_match(label(protection = "fixed_synopsis", mechanism_epsilon = 0.1,
                     noise_reference_epsilon = 0.1),
               "fixed-synopsis protection")
  expect_match(label(protection = "standard",
                     masked_fields = c("person_days", "rate"),
                     masked_reason = "protected_output_required"),
               "unavailable: person_days, rate")
  expect_match(label(protection = "mixed",
                     blocks = list(summary = "standard",
                                   mdrr = "realised_sticky")),
               "mixed: summary standard, mdrr realised_sticky")
})

# --- B12: one logical source counted once -----------------------------------------

test_that("Astra catalogue pooling must reject repeated logical site components", {
  # Astra's review probe (astra-p0-review.md, B12): the same one-site protected
  # payload under two connection names used to pool to N 20 instead of 10.
  df <- data.frame(persons = 10)
  attr(df, "dsomop_release") <- list(protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
    entry = "dsomop:achilles.101", protection = "realised_sticky",
    owners = list(list(
      owner = "review", family = "H", grid = "age_5y", bounds = NULL, cap = 1L,
      allocation = list(N = 0.2, H = 0.8), strata_kind = "root",
      cell_keys = c("a", "b"),
      components = list(list(stratum = "root", n = 10, s = NULL, q = NULL,
                             h = c(4, 6))))))
  expect_error(
    .pool_protected_release_frames(list(connection_alias_A = df,
                                        connection_alias_B = df)),
    "duplicate|logical|independen|overlap|source",
    info = "Catalogue protected payload lacks stable source/component identity")
  # With the logical source identifier both aliases name the same node.
  attr(df, "dsomop_release")$source <- .pc_source(7L)
  expect_error(
    .pool_protected_release_frames(list(connection_alias_A = df,
                                        connection_alias_B = df)),
    "Duplicate logical source: servers connection_alias_A and connection_alias_B")
})

test_that("catalogue pooling needs distinct logical sources, never equal values", {
  # Two independent sites whose answers happen to be equal still pool.
  pooled <- .pool_protected_release_frames(list(
    a = .pc_frame(10, c(7, 3), source = 1L),
    b = .pc_frame(10, c(7, 3), source = 2L)))
  expect_equal(pooled$result$persons, 20)
  expect_equal(attr(pooled$result, "cells")$count, c(14, 6))
  # A third connection to site a is refused, however the values differ.
  expect_error(.pool_protected_release_frames(list(
    a = .pc_frame(10, c(7, 3), source = 1L),
    b = .pc_frame(10, c(7, 3), source = 2L),
    a2 = .pc_frame(12, c(8, 4), source = 1L))),
    "Duplicate logical source: servers a and a2")
  # A site without the identifier cannot be shown to be independent.
  expect_error(.pool_protected_release_frames(list(
    a = .pc_frame(10, c(7, 3), source = 1L),
    b = .pc_frame(10, c(7, 3), source = NULL))),
    "server\\(s\\) b report no logical source identifier")
  # A malformed identifier counts as missing.
  odd <- .pc_frame(10, c(7, 3), source = 2L)
  attr(odd, "dsomop_release")$source$logical_source_id <- "site-b"
  expect_error(.pool_protected_release_frames(list(
    a = .pc_frame(10, c(7, 3), source = 1L), b = odd)),
    "no logical source identifier")
  # One site has nothing to pool against.
  single <- .pool_protected_release_frames(list(
    a = .pc_frame(10, c(7, 3), source = NULL)))
  expect_equal(single$result$persons, 10)
})

test_that("typed releases of one logical source are never pooled twice", {
  statuses <- list(a = .pc_status(), b = .pc_status())
  count <- function(n, source) {
    .pc_payload("count", n, "stratum=root", "N", list(noisy_count = n),
                source = source)
  }
  # Alias connections (distinct status noise domains, same logical source).
  .pc_backend(statuses, list(a = count(10, .pc_source(3L)),
                             b = count(10, .pc_source(3L))),
              function(datasources, sent) {
    expect_error(ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                    datasources),
                 "Duplicate logical source: servers a and b")
    # The per-site results stay available without pooling.
    value <- ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                datasources, pool = FALSE)
    expect_equal(c(value$per_site$a$noisy_count,
                   value$per_site$b$noisy_count), c(10, 10))
    expect_identical(unname(value$meta$privacy$logical_source_id),
                     rep(.pc_source(3L)$logical_source_id, 2L))
  })
  # A shared status noise domain is still refused before any release.
  shared <- list(a = .pc_status(noise_domain_id = paste0("dpn_", strrep("9", 40L))),
                 b = .pc_status(noise_domain_id = paste0("dpn_", strrep("9", 40L))))
  .pc_backend(shared, list(a = count(10, NULL), b = count(10, NULL)),
              function(datasources, sent) {
    expect_error(ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                    datasources), "share a DP noise domain")
    expect_false("omopDpReleaseDS" %in% sent$heads)
  })
  # Two independent sites with coincidentally equal answers pool normally.
  .pc_backend(statuses, list(a = count(10, NULL), b = count(10, NULL)),
              function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                datasources)
    expect_equal(value$pooled$noisy_count, 20)
  })
  # Without identifiers the sites' independence cannot be established.
  .pc_backend(statuses, list(a = count(10, NULL), b = count(10, NULL)),
              fill_source = FALSE, function(datasources, sent) {
    expect_error(ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                    datasources),
                 "independence of the sites cannot be established")
  })
  # A single site needs no identifier to "pool" with itself.
  .pc_backend(list(a = .pc_status()), list(a = count(10, NULL)),
              fill_source = FALSE, function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                datasources)
    expect_equal(value$pooled$noisy_count, 10)
  })
})

# A mocked federated catalogue run of a dsOMOP 2.10.3 protected entry.
.pc_analysis_run <- function(frames, code) {
  conns <- stats::setNames(as.list(rep("FAKE", length(frames))), names(frames))
  disclosure <- list(
    harmonization_contract_version = "dsomop-harmonization-v3",
    age_breaks = seq(0, 85, 5),
    age_semantics = "reference_year_minus_year_of_birth",
    date_semantics = "ISO8601_Gregorian_closed_interval",
    date_granularity = "calendar_day", datetime_timezone = "UTC",
    week_start = "Monday", nfilter_age_range = 5, nfilter_date_range = 30,
    nfilter_band = 5, max_feature_specs = 1000, max_pivot_concepts = 1000,
    max_output_columns = 5000, max_temporal_bins = 10000,
    max_events_per_group = 100, max_filter_depth = 32,
    max_filter_nodes = 1024, max_filter_values = 10000,
    max_plan_outputs = 100, max_analysis_scope_tables = 8)
  assign("omop", list(
    conns = conns, res_symbol = "dsO.fake", outputs = NULL,
    capabilities = stats::setNames(
      rep(list(list(disclosure = disclosure)), length(conns)), names(conns))),
    envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists("omop", envir = dsOMOPClient:::.dsomop_client_env)) {
      rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env)
    })
  meta <- list(
    name = "dsomop:achilles.101", mode = "aggregate",
    pooling_contract = list(version = 1L, strategy = "not_poolable",
                            reason = "Protected release (C2)."),
    release_pooling = list(version = 1L, strategy = "protected_components",
                           protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
                           contract = "dsomop-release-contract-v1",
                           release_contract = "dsomop:achilles.101",
                           class = "C2"))
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      head <- if (is.call(expr)) as.character(expr[[1]]) else ""
      if (identical(head, "omopAnalysisGetDS")) {
        return(stats::setNames(rep(list(meta), length(conns)), names(conns)))
      }
      stats::setNames(frames[names(conns)], names(conns))
    },
    .package = "DSI", .env = parent.frame())
  code()
}

# A protection label as dsOMOP 2.10.3 attaches it to a catalogue frame.
.pc_label <- function(frame, protection = "realised_sticky", ...) {
  fixed <- identical(protection, "fixed_synopsis")
  label <- list(protection = protection,
                guarantee_scope = if (fixed) {
                  "fixed_vector_before_exact_support_gate"
                } else "heuristic_realised_contribution_reuse",
                noise_reference_epsilon = 0.1,
                mechanism_epsilon = if (fixed) 0.12 else NA_real_,
                api_transcript_epsilon = NA_real_,
                privacy_call_quota = "none", history_based_admission = FALSE)
  if (fixed) label$delta <- 0
  attr(frame, "dsomop_protection") <- utils::modifyList(label, list(...))
  frame
}

test_that("ds.omop.analysis.run refuses to pool alias connections of one node", {
  site <- .pc_label(.pc_frame(10, c(7, 3), source = 4L))
  .pc_analysis_run(list(alpha = site, alpha_again = site), function() {
    expect_error(ds.omop.analysis.run("dsomop:achilles.101"),
                 "Duplicate logical source: servers alpha and alpha_again")
    split <- ds.omop.analysis.run("dsomop:achilles.101", type = "split")
    expect_named(split$per_site, c("alpha", "alpha_again"))
    expect_null(split$pooled)
  })
  # Independent sites whose values coincide still pool.
  .pc_analysis_run(list(alpha = .pc_label(.pc_frame(10, c(7, 3), source = 4L)),
                        beta = .pc_label(.pc_frame(10, c(7, 3), source = 5L))),
                   function() {
    res <- ds.omop.analysis.run("dsomop:achilles.101")
    expect_equal(res$pooled$persons, 20)
    expect_equal(attr(res$pooled, "cells")$count, c(14, 6))
  })
})

# --- B14: truthful epsilon and delta claims ----------------------------------------

test_that("typed payloads with inconsistent epsilon or delta claims are refused", {
  statuses <- list(a = .pc_status())
  realised <- .pc_payload("count", 10, "stratum=root", "N",
                          list(noisy_count = 10))
  fixed <- .pc_payload("count", 10, "stratum=root", "N",
                       list(noisy_count = 10), protection = "fixed_synopsis",
                       mechanism_epsilon = 0.02)
  expect_null(realised$delta)
  expect_identical(fixed$delta, 0)
  bad <- list(
    realised_delta = c(realised, list(delta = 0)),
    realised_transcript = utils::modifyList(realised, list(
      api_transcript_epsilon = 0.5)),
    realised_mechanism = utils::modifyList(realised, list(
      mechanism_epsilon = 0.02)),
    fixed_without_delta = within(fixed, rm(delta)),
    fixed_positive_delta = utils::modifyList(fixed, list(delta = 1e-6)),
    fixed_without_epsilon = utils::modifyList(fixed, list(
      mechanism_epsilon = NA_real_)),
    fixed_transcript = utils::modifyList(fixed, list(
      api_transcript_epsilon = 0.02))
  )
  for (name in names(bad)) {
    .pc_backend(statuses, list(a = bad[[name]]), function(datasources, sent) {
      expect_error(ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                      datasources),
                   "Server 'a' returned a protected release", info = name)
    })
  }
  # The truthful forms are accepted; a missing delta is not a claim.
  for (ok in list(realised, fixed,
                  utils::modifyList(realised, list(delta = NA_real_)))) {
    .pc_backend(statuses, list(a = ok), function(datasources, sent) {
      value <- ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                  datasources)
      expect_equal(value$pooled$noisy_count, 10)
    })
  }
})

test_that("catalogue protection labels must make consistent claims", {
  frame <- .pc_frame(10, c(7, 3), source = 4L)
  bad <- list(
    realised_delta = .pc_label(frame, delta = 0),
    realised_epsilon = .pc_label(frame, mechanism_epsilon = 0.1),
    mixed_epsilon = .pc_label(frame, protection = "mixed",
                              mechanism_epsilon = 0.1,
                              blocks = list(binary = "standard",
                                            distribution = "fixed_synopsis")),
    mixed_delta = .pc_label(frame, protection = "mixed", delta = 0),
    fixed_without_delta = .pc_label(frame, protection = "fixed_synopsis",
                                    delta = NULL),
    fixed_transcript = .pc_label(frame, protection = "fixed_synopsis",
                                 api_transcript_epsilon = 0.12)
  )
  for (name in names(bad)) {
    .pc_analysis_run(list(alpha = bad[[name]]), function() {
      expect_error(ds.omop.analysis.run("dsomop:achilles.101", type = "split"),
                   "Server 'alpha' returned a protected protection label",
                   info = name)
    })
  }
  for (ok in list(.pc_label(frame), .pc_label(frame, protection = "fixed_synopsis"),
                  .pc_label(frame, protection = "mixed",
                            blocks = list(binary = "standard",
                                          distribution = "realised_sticky")))) {
    .pc_analysis_run(list(alpha = ok), function() {
      res <- ds.omop.analysis.run("dsomop:achilles.101", type = "split")
      expect_named(res$meta$protection, "alpha")
    })
  }
  # Standard and legacy labels are not 2.10.3 protected claims.
  .pc_analysis_run(list(alpha = .pc_label(frame, protection = "dp",
                                          epsilon = 0.5, delta = 0)),
                   function() {
    res <- ds.omop.analysis.run("dsomop:achilles.101", type = "split")
    expect_identical(res$meta$protection$alpha$protection, "dp")
  })
})

test_that("only a fixed synopsis is printed with a delta", {
  result <- dsomop_result(per_site = list(a = data.frame(x = 1)))
  label <- function(...) {
    result[["meta"]]$protection <- list(a = list(...))
    paste(capture.output(print(result)), collapse = "\n")
  }
  fixed <- label(protection = "fixed_synopsis", mechanism_epsilon = 0.12,
                 delta = 0, noise_reference_epsilon = 0.1)
  expect_match(fixed, "declared vector epsilon 0.12, delta 0")
  realised <- label(protection = "realised_sticky",
                    noise_reference_epsilon = 0.1)
  expect_false(grepl("delta", realised))
  mixed <- label(protection = "mixed",
                 blocks = list(binary = "standard",
                               distribution = "realised_sticky"))
  expect_false(grepl("delta", mixed))
})

test_that("typed views pool from their complete basis (dsOMOP 2.10.3 B05)", {
  statuses <- list(a = .pc_status(), b = .pc_status())
  basis_coords <- c("root", "cell=F", "cell=M", "cell=Other", "cell=Unknown")
  basis_families <- c("N", "H", "H", "H", "H")
  site <- function(basis_signed) {
    view <- list(1L, 2L)
    shown_cells <- vapply(view, function(i) sum(basis_signed[-1L][i]),
                          numeric(1L))
    p <- .pc_payload("categorical_histogram",
                     c(basis_signed[[1L]], shown_cells),
                     c("root", "cell=F", "cell=M"), c("N", "H", "H"),
                     list(levels = c("F", "M"), counts = pmax(0, shown_cells)))
    p$components$basis <- list(coordinates = basis_coords,
                               families = basis_families,
                               signed = basis_signed)
    p$components$view_of_basis <- view
    p
  }
  # Site totals 20 and 30; Other/Unknown cells carry protected mass too.
  releases <- list(a = site(c(20, 9, 8, 2, 1)), b = site(c(30, 12, 14, 3, 1)))
  .pc_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table",
      omop_privacy("categorical_histogram", "sex", levels = c("F", "M")),
      datasources)
    # Pooled basis (50; 21, 22, 5, 2) already sums to 50: the views are its
    # F and M cells, not a re-projection of the displayed view alone.
    expect_equal(value$pooled$counts, c(21, 22))
    expect_identical(value$pooled$pooling,
                     "sum_of_signed_basis_components_then_derived_once")
  })
  # A different basis (e.g. another registered domain) is refused.
  other <- releases
  other$b$components$basis$coordinates[[4L]] <- "cell=X"
  .pc_backend(statuses, other, function(datasources, sent) {
    expect_error(ds.omop.dp.release(
      "analysis_table",
      omop_privacy("categorical_histogram", "sex", levels = c("F", "M")),
      datasources), "not compatible")
  })
})
