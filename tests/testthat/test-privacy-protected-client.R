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

# One protected payload as dsOMOP 2.10.3 returns it.
.pc_payload <- function(statistic, signed, coordinates, families, shown,
                        protection = "realised_sticky", ...) {
  fixed <- identical(protection, "fixed_synopsis")
  utils::modifyList(c(list(
    protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
    release_contract = "dsomop-release-contract-v1",
    statistic = statistic, protection = protection,
    guarantee_scope = if (fixed) "fixed_vector_before_exact_support_gate" else
      "heuristic_realised_contribution_reuse",
    noise_reference_epsilon = 0.1,
    mechanism_epsilon = if (fixed) 0.1 else NA_real_,
    api_transcript_epsilon = NA_real_, delta = 0,
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
                             "none_views_project_onto_the_person_total",
                           signed = signed))), list(...))
}

.pc_backend <- function(statuses, releases, code) {
  for (i in seq_along(statuses)) {
    statuses[[i]]$noise_domain_id <- statuses[[i]]$noise_domain_id %||%
      paste0("dpn_", strrep(as.character(i), 40L))
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
                     "sum_of_signed_components_then_derived_once")
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
    a = .pc_payload("categorical_histogram", c(20, 12, -3), coords,
                    c("N", "H", "H"),
                    list(levels = c("F", "M"), counts = c(16, 4))),
    b = .pc_payload("categorical_histogram", c(30, 10, 25), coords,
                    c("N", "H", "H"),
                    list(levels = c("F", "M"), counts = c(7.5, 22.5))))
  .pc_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table",
      omop_privacy("categorical_histogram", "sex", levels = c("F", "M")),
      datasources)
    # Pooled N = 50; summed cells (22, 22) project onto it as (25, 25).
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

test_that("protected catalogue frames pool their signed components", {
  frame <- function(n, h, protection = "realised_sticky") {
    df <- data.frame(cell = c("0-4", "5-9"), n_persons = pmax(0, h))
    attr(df, "dsomop_release") <- list(
      protocol = .DP_PROTECTED_RELEASE_PROTOCOL,
      contract = "dsomop-release-contract-v1",
      entry = "dsomop:achilles.101", protection = protection,
      reference_epsilon = 0.1,
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
  pooled <- .pool_protected_release_frames(list(
    a = frame(10, c(7, 3)), b = frame(20, c(-5, 25))))
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
  other <- frame(20, c(5, 15))
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
