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
                           # The complete basis and its versioned projection
                           # (dsOMOP 2.10.3, R04/R05).
                           projection = .pc_projection(families),
                           view_kind = "basis",
                           signed = signed,
                           basis = list(
                             id = paste0("typed-", statistic, "-basis/v1"),
                             coordinates = coordinates, families = families,
                             signed = signed)))), list(...))
  if (fixed && !"delta" %in% names(list(...))) payload$delta <- 0
  payload
}

# The server's projection identifier for a basis of these families.
.pc_projection <- function(families) {
  if (length(families) > 1L && identical(families[[1L]], "N") &&
      families[[2L]] %in% c("H")) return("typed-partition-simplex-v1")
  if (length(families) > 1L && identical(families[[1L]], "T")) {
    return("typed-weighted-total-simplex-v1")
  }
  if (length(families) && all(families == "W")) {
    return("typed-weighted-atomwise-clip-v1")
  }
  "none"
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
    p$components$basis <- list(id = "typed-one-label/domain=F,M/v2",
                               coordinates = basis_coords,
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

# --- Record-weighted releases, protected effects, fallback and mixed frames ----

# A mocked federated catalogue run with an explicit entry description.
.pc_analysis_run_meta <- function(frames, meta, code) {
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

.pc_rm_display <- list(
  columns = c("condition_concept_id", "concept_name", "n_persons",
              "n_records", "avg_days", "stddev_days"),
  persons = "n_persons", records = "n_records", mean = "avg_days",
  sd = "stddev_days", order = "n_persons")

# One released stratum of a record-moment owner: lattice sums of the record
# values u on [0, 10957] (without noise, so the pooled moments are exact).
.pc_rm_comp <- function(concept, n, u, moments = TRUE,
                        scale = "record-scale/duration-days-v2",
                        name = paste("concept", concept)) {
  J <- 65535
  list(stratum = paste0("concept=", concept), n = n, r = length(u),
       s = if (moments) sum(round(J * u)) else NULL,
       q = if (moments) sum(round(J * u * u)) else NULL,
       scale = if (moments) scale else NULL,
       bounds = if (moments) c(0, 10957) else NULL,
       display = list(condition_concept_id = concept, concept_name = name),
       order_value = concept, order_unit = 0)
}

.pc_rm_frame <- function(comps, source, protection = "fixed_synopsis",
                         cap = 10L) {
  df <- data.frame(
    condition_concept_id = vapply(comps, function(c) {
      c$display$condition_concept_id
    }, numeric(1L)),
    concept_name = vapply(comps, function(c) c$display$concept_name,
                          character(1L)),
    n_persons = vapply(comps, function(c) max(0, c$n), numeric(1L)),
    n_records = vapply(comps, function(c) max(0, c$r), numeric(1L)),
    avg_days = NA_real_, stddev_days = NA_real_, stringsAsFactors = FALSE)
  owner <- "querylibrary/condition_era.length_stats/record-moments/concept=none/v1"
  attr(df, "dsomop_release") <- list(
    protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
    contract = "dsomop-release-contract-v1",
    entry = "dsomop:condition_era.length_stats", protection = protection,
    reference_epsilon = 0.1, source = .pc_source(source),
    owners = list(list(
      protocol = .DP_PROTECTED_RELEASE_PROTOCOL, owner = owner,
      population = owner, family = "RM", weighting = "record",
      strata_kind = "data", cap = cap, cap_scope = "whole_person_records",
      record_order = c("condition_era_start_date", "condition_era_id"),
      lattice = 65535, normalization = "unit_interval",
      allocation = list(N = 0.2, R = 0.3, S = 0.3, Q = 0.2),
      display = .pc_rm_display, components = comps)))
  df
}

# The record-weighted mean and SD of the pooled values (no projection needed:
# noise-free sums are feasible).
.pc_rm_expected <- function(u) {
  J <- 65535
  R <- length(u)
  S <- sum(round(J * u)) / J
  Q <- sum(round(J * u * u)) / J
  c(mean = 10957 * S / R, sd = 10957 * sqrt((Q - S^2 / R) / (R - 1)))
}

test_that("record-weighted protected releases pool N, R, S and Q per stratum", {
  ua <- (1:60) / 100
  ub <- (11:60) / 80
  u2 <- seq(0.2, 0.6, length.out = 20)
  a <- .pc_rm_frame(list(.pc_rm_comp(1, 30, ua), .pc_rm_comp(2, 12, u2)), 1L)
  b <- .pc_rm_frame(list(.pc_rm_comp(1, 20, ub),
                         .pc_rm_comp(3, 15, rep(0.5, 15), moments = FALSE)),
                    2L)
  strict <- .pool_protected_release_frames(list(a = a, b = b))
  expect_identical(names(strict$result), .pc_rm_display$columns)
  expect_equal(strict$result$condition_concept_id, 1)
  expect_equal(strict$result$n_persons, 50)
  expect_equal(strict$result$n_records, 110)
  want <- .pc_rm_expected(c(ua, ub))
  expect_equal(strict$result$avg_days, unname(want[["mean"]]),
               tolerance = 1e-9)
  expect_equal(strict$result$stddev_days, unname(want[["sd"]]),
               tolerance = 1e-9)
  # The persons never weight the mean: changing N leaves the moments alone.
  a2 <- .pc_rm_frame(list(.pc_rm_comp(1, 3, ua), .pc_rm_comp(2, 12, u2)), 1L)
  other <- .pool_protected_release_frames(list(a = a2, b = b))
  expect_equal(other$result$avg_days, strict$result$avg_days)
  expect_match(paste(strict$warnings, collapse = " "),
               "record-weighted mean and SD derived once")
  # pooled_only_ok also pools strata released by some servers, ordered by
  # pooled persons; a stratum released without moments keeps them NA.
  partial <- .pool_protected_release_frames(list(a = a, b = b),
                                            policy = "pooled_only_ok")
  expect_equal(partial$result$condition_concept_id, c(1, 3, 2))
  expect_equal(partial$result$n_persons, c(50, 15, 12))
  expect_true(is.na(partial$result$avg_days[[2L]]))
  want2 <- .pc_rm_expected(u2)
  expect_equal(partial$result$avg_days[[3L]], unname(want2[["mean"]]),
               tolerance = 1e-9)
  expect_match(paste(partial$warnings, collapse = " "),
               "released by only some servers")
  expect_match(paste(partial$warnings, collapse = " "),
               "Moments were not pooled for 1 strata")
})

test_that("record-weighted pooling refuses incompatible releases and scales", {
  ua <- (1:60) / 100
  ub <- (11:60) / 80
  a <- .pc_rm_frame(list(.pc_rm_comp(1, 30, ua)), 1L)
  b <- .pc_rm_frame(list(.pc_rm_comp(1, 20, ub)), 2L)
  # Another cap is another owner contract.
  capped <- .pc_rm_frame(list(.pc_rm_comp(1, 20, ub)), 2L, cap = 5L)
  expect_error(.pool_protected_release_frames(list(a = a, b = capped)),
               "not compatible")
  # Another public scale: counts pool, moments do not.
  rescaled <- .pc_rm_frame(list(.pc_rm_comp(1, 20, ub, scale = "other")), 2L)
  out <- .pool_protected_release_frames(list(a = a, b = rescaled))
  expect_equal(out$result$n_records, 110)
  expect_true(is.na(out$result$avg_days))
  # Differing labels are kept as NA, with a warning.
  renamed <- .pc_rm_frame(list(.pc_rm_comp(1, 20, ub, name = "other")), 2L)
  out <- .pool_protected_release_frames(list(a = a, b = renamed))
  expect_true(is.na(out$result$concept_name))
  expect_match(paste(out$warnings, collapse = " "), "concept_name")
  # One logical node is never counted twice.
  expect_error(.pool_protected_release_frames(list(a = a, a2 = a)),
               "Duplicate logical source")
  # A record-moment owner never pools with person-weighted components.
  mixed <- a
  rel <- attr(mixed, "dsomop_release")
  rel$owners[[2L]] <- attr(.pc_frame(10, c(7, 3)), "dsomop_release")$owners[[1L]]
  attr(mixed, "dsomop_release") <- rel
  expect_error(.pool_protected_release_frames(list(a = mixed, b = b)),
               "record-weighted protected release cannot be pooled")
  # A server without the pooling fields (an older 2.10.3 build) is refused.
  bare <- b
  rel <- attr(bare, "dsomop_release")
  rel$owners[[1L]]$display <- NULL
  attr(bare, "dsomop_release") <- rel
  expect_error(.pool_protected_release_frames(list(a = bare, b = a)),
               "without its pooling fields")
})

test_that("ds.omop.analysis.run pools record-weighted summaries in their schema", {
  ua <- (1:60) / 100
  ub <- (11:60) / 80
  columns <- stats::setNames(lapply(.pc_rm_display$columns, function(x) {
    list(role = "key")
  }), .pc_rm_display$columns)
  meta <- list(
    name = "dsomop:condition_era.length_stats", mode = "aggregate",
    pooling_contract = list(version = 1L, strategy = "not_poolable",
                            reason = "Protected release (C7)."),
    release_pooling = list(version = 1L, strategy = "protected_components",
                           protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
                           contract = "dsomop-release-contract-v1",
                           release_contract = "dsomop:condition_era.length_stats",
                           class = "C7", columns = columns))
  frames <- list(alpha = .pc_label(.pc_rm_frame(list(.pc_rm_comp(1, 30, ua)),
                                                1L), "fixed_synopsis"),
                 beta = .pc_label(.pc_rm_frame(list(.pc_rm_comp(1, 20, ub)),
                                               2L), "fixed_synopsis"))
  .pc_analysis_run_meta(frames, meta, function() {
    res <- ds.omop.analysis.run("dsomop:condition_era.length_stats")
    expect_identical(names(res$pooled), .pc_rm_display$columns)
    expect_equal(res$pooled$n_records, 110)
    expect_equal(res$pooled$avg_days,
                 unname(.pc_rm_expected(c(ua, ub))[["mean"]]),
                 tolerance = 1e-9)
  })
})

# One protected cm.effect_estimate frame: per-arm C6 components and the
# protected estimate repeated on both arm rows.
.pc_effect_frame <- function(le, se, source, persons = c(100, 90),
                             events = c(20, 15), time = c(1000, 900)) {
  df <- data.frame(
    arm = c("target", "comparator"), model_type = "poisson",
    persons = persons, person_days = time, outcomes = events,
    estimate = exp(le), ci_lo = exp(le - 1.96 * se),
    ci_hi = exp(le + 1.96 * se), log_estimate = le, se_log_estimate = se,
    stringsAsFactors = FALSE)
  comps <- lapply(1:2, function(i) {
    list(stratum = c("arm=target", "arm=comparator")[[i]], n = persons[[i]],
         status = c(events[[i]], persons[[i]] - events[[i]]), time = time[[i]])
  })
  attr(df, "dsomop_release") <- list(
    protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
    contract = "dsomop-release-contract-v1",
    entry = "dsomop:cm.effect_estimate", protection = "realised_sticky",
    reference_epsilon = 0.1, source = .pc_source(source),
    owners = list(list(
      protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
      owner = "cohortmethod/first-outcome-person-time/v2",
      population = "c6/persons-at-risk/v1", family = "C6",
      strata_kind = "independent", cap = 1L, horizon_days = 3650,
      allocation = list(N = 0.2, status = 0.3, time = 0.5),
      components = comps)))
  .pc_label(df)
}

.pc_effect_contract <- function() {
  cols <- c("arm", "model_type", "persons", "person_days", "outcomes",
            "estimate", "ci_lo", "ci_hi", "log_estimate", "se_log_estimate")
  columns <- stats::setNames(lapply(cols, function(x) {
    if (x %in% c("arm", "model_type")) list(role = "key")
    else if (x %in% c("persons", "person_days", "outcomes")) list(role = "sum")
    else list(role = "nonpoolable", reason = "Pooled by inverse variance.")
  }), cols)
  list(version = 1L, strategy = "effect_estimate", columns = columns,
       strata = c("arm", "model_type"), log_estimate = "log_estimate",
       standard_error = "se_log_estimate", transform = "exp")
}

.pc_effect_meta <- function(effect = TRUE) {
  contract <- .pc_effect_contract()
  list(
    name = "dsomop:cm.effect_estimate", mode = "aggregate",
    pooling_contract = list(version = 1L, strategy = "not_poolable",
                            reason = "Protected release (C9)."),
    release_pooling = c(list(
      version = 1L, strategy = "protected_components",
      protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
      contract = "dsomop-release-contract-v1",
      release_contract = "dsomop:cm.effect_estimate", class = "C9",
      columns = contract$columns, standard_contract = contract),
      if (effect) list(effect_estimate = contract)))
}

test_that("protected effect estimates pool by meta-analysis of the site estimates", {
  frames <- list(nairobi = .pc_effect_frame(log(1.5), 0.2, 1L),
                 douala = .pc_effect_frame(log(2.0), 0.3, 2L,
                                           persons = c(120, 110),
                                           events = c(24, 18),
                                           time = c(1200, 1100)))
  .pc_analysis_run_meta(frames, .pc_effect_meta(), function() {
    res <- ds.omop.meta.effect_estimate(
      params = list(outcome_concept_id = 4329847), cohort = c(1L, 2L))
    w <- 1 / c(0.2, 0.3)^2
    fixed <- exp(sum(w * log(c(1.5, 2))) / sum(w))
    expect_true(all(res$pooled$n_databases == 2L))
    expect_equal(res$pooled$estimate_fixed, rep(fixed, 2L), tolerance = 1e-9)
    target <- res$pooled[res$pooled$arm == "target", , drop = FALSE]
    expect_equal(target$persons, 220)
    # The signed-component pooling stays available.
    comps <- attr(res$pooled, "protected_components")
    expect_equal(comps$persons, c(220, 200))
    expect_equal(comps$events, c(44, 33))
    expect_match(paste(res$meta$warnings, collapse = " "),
                 "inverse-variance meta-analysis of the per-site protected")
  })
  # An unavailable site estimate fails a strict pool closed; pooled_only_ok
  # meta-analyses the others.
  frames$douala$log_estimate <- NA_real_
  frames$douala$se_log_estimate <- NA_real_
  .pc_analysis_run_meta(frames, .pc_effect_meta(), function() {
    strict <- ds.omop.meta.effect_estimate(
      params = list(outcome_concept_id = 4329847), cohort = c(1L, 2L))
    expect_null(strict$pooled)
    loose <- ds.omop.meta.effect_estimate(
      params = list(outcome_concept_id = 4329847), cohort = c(1L, 2L),
      pooling_policy = "pooled_only_ok")
    expect_true(all(loose$pooled$n_databases == 1L))
    expect_equal(loose$pooled$estimate_fixed, rep(1.5, 2L), tolerance = 1e-9)
  })
  # Without the effect descriptor the entry pools its components only.
  frames <- list(nairobi = .pc_effect_frame(log(1.5), 0.2, 1L),
                 douala = .pc_effect_frame(log(2.0), 0.3, 2L))
  .pc_analysis_run_meta(frames, .pc_effect_meta(effect = FALSE), function() {
    res <- ds.omop.analysis.run("dsomop:cm.effect_estimate",
                                cohort = c(1L, 2L))
    expect_equal(res$pooled$persons, c(200, 180))
    expect_null(res$pooled$estimate_fixed)
  })
})

test_that("standard outputs of servers without DP pool by the established contract", {
  plain <- function(le, se) {
    df <- .pc_effect_frame(le, se, 1L)
    attr(df, "dsomop_release") <- NULL
    attr(df, "dsomop_protection") <- list(protection = "standard_fallback",
                                          reason = "dp_unavailable")
    df
  }
  frames <- list(a = plain(log(1.5), 0.2), b = plain(log(2), 0.3))
  .pc_analysis_run_meta(frames, .pc_effect_meta(), function() {
    res <- ds.omop.meta.effect_estimate(
      params = list(outcome_concept_id = 4329847), cohort = c(1L, 2L))
    expect_true(all(res$pooled$n_databases == 2L))
    expect_match(paste(res$meta$warnings, collapse = " "),
                 "No server returned protected components")
  })
  # One protected and one standard server are still never mixed.
  frames$a <- .pc_effect_frame(log(1.5), 0.2, 1L)
  .pc_analysis_run_meta(frames, .pc_effect_meta(), function() {
    expect_error(ds.omop.meta.effect_estimate(
      params = list(outcome_concept_id = 4329847), cohort = c(1L, 2L)),
      "cannot be pooled together")
  })
})

test_that("pooled mixed frames say their protected fields are not exact", {
  mixed <- function(days) {
    df <- data.frame(arm = c("target", "comparator"), persons = c(100, 90),
                     person_days = days, outcomes = c(20, 15),
                     mdrr = c(1.8, 1.8), stringsAsFactors = FALSE)
    # As dsOMOP 2.10.3 labels it: the protected block's guarantee scope.
    attr(df, "dsomop_protection") <- list(
      protection = "mixed",
      guarantee_scope = "heuristic_realised_contribution_reuse",
      blocks = list(standard = c("arm", "persons", "outcomes"),
                    person_time = "realised_sticky"))
    df
  }
  columns <- list(arm = list(role = "key"), persons = list(role = "sum"),
                  person_days = list(role = "sum"),
                  outcomes = list(role = "sum"),
                  mdrr = list(role = "nonpoolable", reason = "Per site."))
  meta <- list(name = "dsomop:cm.mdrr", mode = "aggregate",
               pooling_contract = list(version = 1L, strategy = "tabular",
                                       columns = columns))
  frames <- list(a = mixed(c(1000, 900)), b = mixed(c(1100, 950)))
  .pc_analysis_run_meta(frames, meta, function() {
    res <- ds.omop.analysis.run("dsomop:cm.mdrr", cohort = c(1L, 2L))
    expect_equal(res$pooled$person_days, c(2100, 1850))
    expect_match(paste(res$meta$warnings, collapse = " "),
                 "pooled person_days, mdrr are sums or derivations")
  })
})

test_that("the client's moment projection is the server's, candidate order too", {
  # dsOMOP 2.10.3 .dsomopReleaseProjectMoments, verbatim.
  server <- function(S0, Q0, N) {
    if (!is.finite(N) || N <= 0) return(c(S = 0, Q = 0))
    feasible <- function(S, Q) {
      Q >= 0 && Q <= N + 1e-12 * N && S * S <= N * Q + 1e-9 * N
    }
    if (feasible(S0, Q0)) return(c(S = S0, Q = Q0))
    candidates <- list(c(-N, N), c(N, N))
    if (abs(S0) <= N) candidates[[length(candidates) + 1L]] <- c(S0, N)
    g <- function(S) (S - S0) + 2 * S * (S * S / N - Q0) / N
    lo <- min(0, S0)
    hi <- max(0, S0)
    if (g(lo) * g(hi) <= 0) {
      for (i in seq_len(200L)) {
        mid <- (lo + hi) / 2
        if (g(lo) * g(mid) <= 0) hi <- mid else lo <- mid
      }
      s <- max(-N, min(N, (lo + hi) / 2))
      candidates[[length(candidates) + 1L]] <- c(s, s * s / N)
    }
    candidates[[length(candidates) + 1L]] <- c(0, 0)
    distance <- vapply(candidates, function(p) (p[[1L]] - S0)^2 +
                         (p[[2L]] - Q0)^2, numeric(1L))
    ok <- vapply(candidates, function(p) feasible(p[[1L]], p[[2L]]),
                 logical(1L))
    best <- which(ok)[which.min(distance[ok])]
    point <- candidates[[best]]
    c(S = point[[1L]], Q = max(0, min(N, point[[2L]])))
  }
  grid <- expand.grid(S0 = seq(-30, 30, by = 2.5), Q0 = seq(-20, 40, by = 2.5),
                      N = c(1, 7, 10, 25))
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    expect_identical(.dp_project_moments(g$S0, g$Q0, g$N),
                     server(g$S0, g$Q0, g$N), info = paste(unlist(g)))
  }
})

# --- Round 3 (re-review R04, R05, B14, R08) -----------------------------------------

# A one-label binary payload on the complete basis N + cells (a, b, Other,
# Unknown), with its public numerator and denominator masks.
.pc_binary <- function(basis_signed, positive = 2L, nonmissing = FALSE,
                       domain = c("a", "b"), source = 1L) {
  cells <- basis_signed[-1L]
  view <- list(as.integer(positive),
               if (nonmissing) 1:3 else 1:4)
  h <- .dp_simplex(cells, max(0, basis_signed[[1L]]))
  num <- sum(h[view[[1L]]]); den <- sum(h[view[[2L]]])
  p <- .pc_payload(
    "binary_rate", c(basis_signed[[1L]], sum(cells[view[[1L]]]),
                     sum(cells[view[[2L]]])),
    c("stratum=root", "stratum=root|view=numerator",
      "stratum=root|view=denominator"), c("N", "H", "H"),
    list(noisy_numerator = num, noisy_denominator = den,
         value = if (den > 0) num / den else NULL, reducer = "first",
         denominator = if (nonmissing) "nonmissing" else "all_persons",
         value_type = "categorical_utf8_v1"))
  p$components$owner <- "typed/categorical-one-label/reducer=first/v2"
  p$components$projection <- "typed-partition-simplex-v1"
  p$components$view_kind <- "binary_rate_numerator_denominator"
  p$components$view_of_basis <- view
  p$components$basis <- list(
    id = paste0("typed-one-label/reducer=first/domain=",
                paste(domain, collapse = ","), "/v2"),
    domain = domain,
    coordinates = c("stratum=root", paste0("stratum=root|cell=", domain),
                    "stratum=root|other", "stratum=root|unknown"),
    families = c("N", "H", "H", "H", "H"), signed = basis_signed)
  p$source <- .pc_source(source)
  p
}

test_that("binary rates pool their numerator and denominator masks (R04)", {
  statuses <- list(a = .pc_status(), b = .pc_status())
  privacy <- omop_privacy("binary_rate", "label", positive = "b",
                          reducer = "first", order_by = "day",
                          denominator = "nonmissing")
  a <- .pc_binary(c(100, 40, 35, 15, 10), nonmissing = TRUE)
  b <- .pc_binary(c(60, 20, 25, 5, 10), nonmissing = TRUE, source = 2L)
  # One site: the pool reproduces the per-site display exactly.
  one <- .dp_pool_protected(list(a = a), privacy)
  expect_equal(one$noisy_numerator, a$noisy_numerator, tolerance = 1e-12)
  expect_equal(one$noisy_denominator, a$noisy_denominator, tolerance = 1e-12)
  .pc_backend(statuses, list(a = a, b = b), function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", privacy, datasources)
    h <- .dp_simplex(c(60, 60, 20, 20), 160)
    expect_equal(value$pooled$noisy_numerator, h[[2L]], tolerance = 1e-12)
    expect_equal(value$pooled$noisy_denominator, sum(h[1:3]),
                 tolerance = 1e-12)
    expect_equal(value$pooled$estimate, h[[2L]] / sum(h[1:3]),
                 tolerance = 1e-12)
  })
  # Equal-width domains with other labels, other masks, a payload without
  # masks: never pooled.
  z <- .pc_binary(c(60, 20, 25, 5, 10), nonmissing = TRUE, domain = c("b", "z"),
                  source = 2L)
  expect_error(.dp_pool_protected(list(a = a, b = z), privacy),
               "not compatible")
  all_persons <- .pc_binary(c(60, 20, 25, 5, 10), source = 2L)
  expect_error(.dp_pool_protected(list(a = a, b = all_persons), privacy),
               "not compatible")
  bare <- a
  bare$components$view_of_basis <- NULL
  bare$components$view_kind <- "basis"
  .pc_backend(list(a = .pc_status()), list(a = bare),
              function(datasources, sent) {
    expect_error(ds.omop.dp.release("analysis_table", privacy, datasources),
                 "numerator and denominator masks")
  })
})

test_that("weighted histograms pool through their protected total (R05)", {
  statuses <- list(a = .pc_status(), b = .pc_status())
  site <- function(total, atoms, source) {
    view <- list(1:2, 3L)
    h <- .dp_simplex(atoms, max(0, total))
    p <- .pc_payload(
      "numeric_histogram", c(total, sum(atoms[1:2]), atoms[[3L]]),
      c("stratum=root|records", "stratum=root|bin=[-inf,1)",
        "stratum=root|bin=[1,inf)"), c("T", "W", "W"),
      list(breaks = c(0, 1, 2), counts = c(sum(h[1:2]), h[[3L]]),
           reducer = "records", max_contributions = 1L,
           value_type = "number"))
    p$components$projection <- "typed-weighted-total-simplex-v1"
    p$components$view_kind <- "sums_of_basis_cells"
    p$components$view_of_basis <- view
    p$components$basis <- list(
      id = "typed-number-signed-nice-v1",
      coordinates = c("stratum=root|records", "stratum=root|atom=[-inf,0)",
                      "stratum=root|atom=[0,1)", "stratum=root|atom=[1,inf)"),
      families = c("T", "W", "W", "W"), signed = c(total, atoms))
    p$source <- .pc_source(source)
    p
  }
  a <- site(50, c(-30, 40, 40), 1L)
  b <- site(70, c(10, -20, 80), 2L)
  privacy <- omop_privacy("numeric_histogram", "x", breaks = c(0, 1, 2),
                          reducer = "records", max_contributions = 1L)
  one <- .dp_pool_protected(list(a = a), privacy)
  expect_equal(one$counts, a$counts, tolerance = 1e-12)
  .pc_backend(statuses, list(a = a, b = b), function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", privacy, datasources)
    h <- .dp_simplex(c(-20, 20, 120), 120)
    expect_equal(value$pooled$counts, c(sum(h[1:2]), h[[3L]]),
                 tolerance = 1e-12)
    expect_identical(value$pooled$projection,
                     "typed-weighted-total-simplex-v1")
  })
  # An unknown projection is refused before pooling.
  odd <- a
  odd$components$projection <- "typed-weighted-something-v9"
  .pc_backend(list(a = .pc_status()), list(a = odd),
              function(datasources, sent) {
    expect_error(ds.omop.dp.release("analysis_table", privacy, datasources),
                 "known projection")
  })
})

test_that("catalogue labels and typed payloads share one scope check (B14)", {
  base <- list(protection = "realised_sticky",
               guarantee_scope = "heuristic_realised_contribution_reuse",
               mechanism_epsilon = NA_real_, api_transcript_epsilon = NA_real_)
  frame <- function(label) {
    x <- data.frame(count = 10)
    attr(x, "dsomop_protection") <- label
    list(site = x)
  }
  expect_length(.analysis_protection_labels(frame(base)), 1L)
  bad <- list(
    realised_claiming_fixed = utils::modifyList(base, list(
      guarantee_scope = "fixed_vector_before_exact_support_gate")),
    realised_without_scope = utils::modifyList(base, list(
      guarantee_scope = NULL)),
    fixed_without_scope = list(protection = "fixed_synopsis",
                               mechanism_epsilon = 0.1, delta = 0),
    mixed_without_blocks = list(
      protection = "mixed",
      guarantee_scope = "heuristic_realised_contribution_reuse"),
    mixed_two_routes = list(
      protection = "mixed",
      guarantee_scope = "heuristic_realised_contribution_reuse",
      blocks = list(a = "realised_sticky", b = "fixed_synopsis")),
    mixed_wrong_scope = list(
      protection = "mixed",
      guarantee_scope = "fixed_vector_before_exact_support_gate",
      blocks = list(standard = c("x", "y"), person_time = "realised_sticky")),
    standard_claiming_scope = list(
      protection = "standard",
      guarantee_scope = "fixed_vector_before_exact_support_gate"),
    standard_claiming_delta = list(protection = "standard_fallback",
                                   delta = 0))
  for (name in names(bad)) {
    expect_error(.analysis_protection_labels(frame(bad[[name]])),
                 "guarantee scope|protected block|claims", info = name)
  }
  # A coherent mixed frame (the server's shape) and a legacy DP label pass.
  ok_mixed <- list(
    protection = "mixed",
    guarantee_scope = "fixed_vector_before_exact_support_gate",
    mechanism_epsilon = NA_real_,
    blocks = list(binary = "standard", distribution = "fixed_synopsis"))
  expect_length(.analysis_protection_labels(frame(ok_mixed)), 1L)
  legacy <- list(protection = "dp", epsilon = 0.1, delta = 0)
  expect_length(.analysis_protection_labels(frame(legacy)), 1L)
  # Typed payloads use the same check.
  statuses <- list(a = .pc_status())
  wrong <- .pc_payload("count", 10, "stratum=root", "N",
                       list(noisy_count = 10),
                       guarantee_scope = "fixed_vector_before_exact_support_gate")
  .pc_backend(statuses, list(a = wrong), function(datasources, sent) {
    expect_error(ds.omop.dp.release("analysis_table", omop_privacy("count"),
                                    datasources), "guarantee scope")
  })
})

test_that("Astra: protected NA pooling skips with an explicit warning", {
  frames <- list(a=.pc_effect_frame(log(1.5), .2, 1L),
                 b=.pc_effect_frame(NA_real_, NA_real_, 2L))
  .pc_analysis_run_meta(frames, .pc_effect_meta(), function() {
    loose <- ds.omop.meta.effect_estimate(params=list(outcome_concept_id=4329847),
      cohort=c(1L,2L), pooling_policy="pooled_only_ok")
    expect_true(all(loose$pooled$n_databases == 1L))
    expect_equal(loose$pooled$estimate_fixed, rep(1.5,2L), tolerance=1e-9)
    expect_match(paste(loose$meta$warnings,collapse=" "), "Dropped server.*b")
    message("ASTRA NA warning: ", paste(loose$meta$warnings,collapse=" | "))
  })
})

test_that("Astra: real 2.10.2 client validator rejects current protected status", {
  old <- new.env(parent=environment())
  sys.source(test_path("fixtures/astra-client-2.10.2-privacy.R"),old)
  error <- tryCatch({old$.dp_status_shape(.pc_status(),"newserver");NULL},
                    error=conditionMessage)
  message("ASTRA old-client/new-server error: ", error)
  expect_match(error,"incoherent v3 DP contract field 'history_dependent'")
  # Its actual 2.10.2 pooling code respects the current server's not_poolable fallback.
  sys.source(test_path("fixtures/astra-client-2.10.2-pooling.R"),old)
  out <- old$.pool_analysis_contract(list(site=.pc_effect_frame(log(1.5),.2,1L)),
    .pc_effect_meta()$pooling_contract)
  expect_null(out$result)
  expect_match(paste(out$warnings,collapse=" "), "Protected release")
})

test_that("the 2.10.2 client shows a server's DP status refusal verbatim (R08)", {
  # dsOMOP 2.10.3 refuses status calls without the client contract (server
  # suite: test-privacy-typed-views.R); the old client's own transport code
  # surfaces that message, so the analyst reads the upgrade instruction.
  old <- new.env(parent = environment())
  sys.source(test_path("fixtures/astra-client-2.10.2-privacy.R"), old)
  refusal <- paste0("This server runs dsOMOP 2.10.3, whose DP releases are ",
                    "protected components ... Upgrade dsOMOPClient to 2.10.3 ",
                    "or later to use ds.omop.dp.status()")
  sent <- list()
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, error = NULL, ...) {
      sent[[length(sent) + 1L]] <<- expr
      if (is.function(error)) error(names(conns)[[1L]], refusal)
      list()
    }, .package = "DSI")
  message <- tryCatch({
    old$ds.omop.dp.status(datasources = list(newserver = "FAKE"))
    NA_character_
  }, error = conditionMessage)
  expect_match(message, "Upgrade dsOMOPClient to 2.10.3", fixed = TRUE)
  # The 2.10.2 client sends no client contract.
  expect_identical(length(sent[[1L]]), 1L)
})

test_that("DP status negotiates the contract and flags pre-2.10.3 servers (R08)", {
  legacy <- .pc_status(privacy_contract = "fixed_per_release_seed_data_versioned_v1",
                       release_binding = "seed_data_versioned_v1",
                       privacy_guarantee = .DP_PRIVACY_GUARANTEE)
  legacy$history_based_admission <- NULL
  statuses <- list(new = .pc_status(noise_domain_id = paste0("dpn_", strrep("1", 40L))),
                   old = legacy)
  statuses$old$noise_domain_id <- paste0("dpn_", strrep("2", 40L))
  sent <- list()
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, error = NULL, ...) {
      server <- names(conns)[[1L]]
      sent[[length(sent) + 1L]] <<- list(server = server, expr = expr)
      if (identical(server, "old") && length(expr) > 1L) {
        # A dsOMOP 2.10.2 omopDpStatusDS takes no argument.
        if (is.function(error)) {
          error(server, paste0("Error in omopDpStatusDS(\"",
                               .DP_PROTECTED_RELEASE_PROTOCOL, "\") : unused ",
                               "argument (\"", .DP_PROTECTED_RELEASE_PROTOCOL,
                               "\")"))
        }
        return(list())
      }
      stats::setNames(list(statuses[[server]]), server)
    }, .package = "DSI")
  datasources <- list(new = "FAKE_1", old = "FAKE_2")
  expect_warning(value <- ds.omop.dp.status(datasources),
                 "Server 'old' runs dsOMOP 2.10.2 or older")
  expect_identical(attr(value, "unpatched"), "old")
  expect_identical(value$new$privacy_contract, .DP_PROTECTED_PRIVACY_CONTRACT)
  expect_identical(value$old$privacy_contract,
                   "fixed_per_release_seed_data_versioned_v1")
  # The new server is asked once, with the client contract; the old one is
  # asked again without it.
  calls <- vapply(sent, function(s) paste(s$server, length(s$expr)),
                  character(1L))
  expect_identical(calls, c("new 2", "old 2", "old 1"))
  expect_identical(sent[[1L]]$expr[[2L]], .DP_PROTECTED_RELEASE_PROTOCOL)
  # Releases mixing the two contracts are refused when pooled.
  expect_error(suppressWarnings(
    ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources)),
    "cannot be combined")
})

test_that("catalogue results of pre-2.10.3 servers carry a plain warning (R08)", {
  expect_identical(.dsomop_unpatched_server_warnings(
    c(a = "2.10.3", b = "2.10.4", c = NA_character_)), character(0))
  warned <- .dsomop_unpatched_server_warnings(c(a = "2.10.2", b = "2.10.3"))
  expect_length(warned, 1L)
  expect_match(warned, "Server 'a' runs dsOMOP 2.10.2, older than 2.10.3")
  expect_match(warned, "cannot repair them")
  frames <- list(a = .pc_effect_frame(log(1.5), .2, 1L),
                 b = .pc_effect_frame(log(1.4), .2, 2L))
  .pc_analysis_run_meta(frames, .pc_effect_meta(), function() {
    session <- get("omop", envir = dsOMOPClient:::.dsomop_client_env)
    session$server_versions <- c(a = "2.10.2", b = "2.10.3")
    assign("omop", session, envir = dsOMOPClient:::.dsomop_client_env)
    expect_warning(res <- ds.omop.meta.effect_estimate(
      params = list(outcome_concept_id = 4329847), cohort = c(1L, 2L)),
      "Server 'a' runs dsOMOP 2.10.2")
    expect_match(paste(res$meta$warnings, collapse = " "),
                 "Server 'a' runs dsOMOP 2.10.2, older than 2.10.3")
    expect_false(any(grepl("Server 'b' runs", res$meta$warnings)))
  })
})
