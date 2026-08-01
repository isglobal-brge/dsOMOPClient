.dp_status <- function(epsilon = 0.1,
                       accounting_mode = "bounded_accounted",
                       allocator = NULL, total_epsilon = 1,
                       never_budget_blocked = TRUE,
                       disjoint_persons = NULL,
                       snapshot_id = "snapshot-2026-08-01") {
  accounted <- identical(accounting_mode, "bounded_accounted")
  if (is.null(allocator)) {
    allocator <- if (accounted) {
      "normalized_capped_zeta2_no_block_nominal_v1"
    } else {
      "fixed_epsilon_unbounded_composition_v1"
    }
  }
  value <- list(
    enabled = TRUE,
    ready = TRUE,
    formal_dp = FALSE,
    sticky_noise = TRUE,
    protocol = "dsomop-dp-release-v1",
    canonical_protocol = "dsomop-dp-canonical-json-v1",
    mechanism = "dsomop-sticky-discrete-laplace-prf-v1",
    sampler = "hmac-inverse-cdf-52bit-v1",
    sampler_certified = FALSE,
    privacy_guarantee = "sticky_noise_not_formally_certified_dp",
    epsilon_semantics = "nominal_noise_calibration_not_certified_dp",
    delta_semantics = "no_formal_delta_claim",
    person_local_provenance_required = TRUE,
    provenance_protocol = "dsomop-dp-person-local-provenance-v2",
    adjacency = "add_remove_person",
    snapshot_id = snapshot_id,
    accounting_mode = accounting_mode,
    allocator = allocator,
    total_epsilon = total_epsilon,
    total_delta = 0,
    release_epsilon = epsilon,
    release_delta = 0,
    bounded_accounting = accounted,
    bounded_composition = FALSE,
    never_budget_blocked = never_budget_blocked,
    budget_behavior = if (accounted) {
      "degrade_to_data_independent_zero_no_error"
    } else {
      "fixed_epsilon_no_budget_exhaustion_error_unbounded_composition"
    },
    supported_statistics = c(
      "count", "categorical_histogram", "numeric_histogram",
      "bounded_mean", "binary_rate"
    ),
    longitudinal_contract = "deterministic_person_bounding_v1",
    privacy_epoch = 1L,
    next_release_epsilon = epsilon,
    next_release_degraded = FALSE,
    max_levels = 100L,
    max_contributions = 10L,
    numeric_grid = 100L
  )
  if (!is.null(disjoint_persons)) {
    value$disjoint_persons <- disjoint_persons
  }
  value
}

.dp_release <- function(statistic, epsilon = 0.1, degraded = FALSE,
                        accounting_mode = "bounded_accounted",
                        allocator = NULL, ...) {
  if (isTRUE(degraded) && missing(epsilon)) epsilon <- 0
  if (is.null(allocator)) {
    allocator <- if (identical(accounting_mode, "bounded_accounted")) {
      "normalized_capped_zeta2_no_block_nominal_v1"
    } else "fixed_epsilon_unbounded_composition_v1"
  }
  defaults <- switch(
    statistic,
    count = list(),
    categorical_histogram = list(
      reducer = "presence", max_contributions = 1L,
      value_type = "categorical_utf8_v1"
    ),
    numeric_histogram = list(
      reducer = "mean", max_contributions = 1L, value_type = "number",
      interval_contract = "left_closed_right_open_last_closed"
    ),
    bounded_mean = list(reducer = "mean", value_type = "number"),
    binary_rate = list(
      reducer = "any", denominator = "all_persons",
      value_type = "categorical_utf8_v1"
    )
  )
  payload <- utils::modifyList(defaults, list(...))
  if (identical(statistic, "bounded_mean") && !"value" %in% names(payload)) {
    estimate <- if (payload$noisy_count > 0) {
      payload$lower + (payload$upper - payload$lower) *
        payload$noisy_sum_grid / (payload$noisy_count * payload$numeric_grid)
    } else NULL
    payload["value"] <- list(estimate)
  }
  if (identical(statistic, "binary_rate") && !"value" %in% names(payload)) {
    estimate <- if (payload$noisy_denominator > 0) {
      payload$noisy_numerator / payload$noisy_denominator
    } else NULL
    payload["value"] <- list(estimate)
  }
  sensitivity <- switch(
    statistic,
    count = list(l1 = 1, unit = "person"),
    categorical_histogram = list(
      l1 = payload$max_contributions, unit = "person"
    ),
    numeric_histogram = list(
      l1 = payload$max_contributions, unit = "person"
    ),
    bounded_mean = list(
      count = 1, sum_grid = payload$numeric_grid,
      allocation = "sequential_half_epsilon", unit = "person"
    ),
    binary_rate = list(
      numerator = 1, denominator = 1,
      allocation = "sequential_half_epsilon", unit = "person"
    )
  )
  c(payload, list(
    protocol = "dsomop-dp-release-v1",
    mechanism = "dsomop-sticky-discrete-laplace-prf-v1",
    adjacency = "add_remove_person",
    epsilon = epsilon,
    delta = 0,
    accounting_mode = accounting_mode,
    allocator = allocator,
    sticky = TRUE,
    formal_dp = FALSE,
    sampler = "hmac-inverse-cdf-52bit-v1",
    sampler_certified = FALSE,
    epsilon_semantics = "nominal_noise_calibration_not_certified_dp",
    delta_semantics = "no_formal_delta_claim",
    sensitivity = sensitivity,
    degraded = degraded,
    statistic = statistic
  ))
}

.dp_disclosure <- function(timezone = "UTC", age_breaks = seq(0, 85, 5)) {
  list(
    harmonization_contract_version = "dsomop-harmonization-v3",
    age_breaks = age_breaks,
    age_semantics = "reference_year_minus_year_of_birth",
    date_semantics = "ISO8601_Gregorian_closed_interval",
    date_granularity = "calendar_day",
    datetime_timezone = timezone,
    week_start = "Monday",
    nfilter_age_range = 5,
    nfilter_date_range = 30,
    nfilter_band = 5,
    max_feature_specs = 1000,
    max_pivot_concepts = 1000,
    max_output_columns = 5000,
    max_temporal_bins = 10000,
    max_filter_depth = 32,
    max_filter_nodes = 1024,
    max_filter_values = 10000,
    max_plan_outputs = 100,
    max_analysis_scope_tables = 8
  )
}

.with_dp_backend <- function(statuses, releases, code, disclosures = NULL) {
  datasources <- stats::setNames(
    as.list(paste0("FAKE_", seq_along(statuses))), names(statuses)
  )
  if (is.null(disclosures)) {
    disclosures <- stats::setNames(
      rep(list(.dp_disclosure()), length(statuses)), names(statuses)
    )
  }
  sent <- new.env(parent = emptyenv())
  sent$expressions <- list()
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      sent$expressions <- c(sent$expressions, list(expr))
      head <- if (is.call(expr)) as.character(expr[[1L]]) else ""
      value <- if (identical(head, "omopDpStatusDS")) {
        statuses[[server]]
      } else if (identical(head, "omopDisclosureSettingsDS")) {
        disclosures[[server]]
      } else if (identical(head, "omopDpReleaseDS")) {
        release <- releases[[server]]
        if (inherits(release, "error")) stop(conditionMessage(release),
                                              call. = FALSE)
        release
      } else stop("unexpected method", call. = FALSE)
      stats::setNames(list(value), server)
    },
    .package = "DSI", .env = parent.frame()
  )
  code(datasources, sent)
}

test_that("omop_privacy exposes no analyst-controlled noise state", {
  args <- names(formals(omop_privacy))
  expect_false(any(c("epsilon", "delta", "seed", "nonce", "epoch",
                     "reset", "force") %in% args))

  count <- omop_privacy("count")
  expect_s3_class(count, "omop_privacy")
  expect_identical(count$max_contributions, 1L)
  expect_null(count$population_id)

  named_count <- omop_privacy(
    "count", population_id = "study/site:adults-v1"
  )
  expect_identical(named_count$population_id, "study/site:adults-v1")

  categorical <- omop_privacy(
    "categorical_histogram", "sex",
    levels = c("Unknown", "Male", "Female"), max_contributions = 2L
  )
  expect_identical(categorical$levels, c("Female", "Male", "Unknown"))
  expect_identical(categorical$reducer, "presence")

  numeric <- omop_privacy(
    "numeric_histogram", "value_as_number", breaks = c(0, 10, 20),
    reducer = "records", max_contributions = 3L
  )
  expect_identical(numeric$breaks, c(0, 10, 20))

  dates <- omop_privacy(
    "numeric_histogram", "event_date",
    breaks = c("2020-01-01", "2020-07-01", "2021-01-01")
  )
  expect_identical(dates$breaks,
                   c("2020-01-01", "2020-07-01", "2021-01-01"))
  expect_identical(dates$reducer, "mean")

  datetimes <- omop_privacy(
    "numeric_histogram", "event_datetime",
    breaks = c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z")
  )
  expect_identical(datetimes$breaks,
                   c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z"))

  mean <- omop_privacy(
    "bounded_mean", "value_as_number", lower = 0, upper = 300,
    reducer = "mean"
  )
  expect_identical(mean$lower, 0)

  rate <- omop_privacy(
    "binary_rate", "status", positive = c("yes", "probable", "yes"),
    denominator = "nonmissing"
  )
  expect_identical(rate$positive, c("probable", "yes"))
  expect_identical(rate$denominator, "nonmissing")
})

test_that("omop_privacy rejects ambiguous or unbounded specifications", {
  expect_error(omop_privacy("co"), "must be one of")
  expect_error(omop_privacy("count", variable = "person_id"), "not valid")
  expect_error(
    omop_privacy("categorical_histogram", "sex", levels = c("F", "F")),
    "unique"
  )
  expect_error(
    omop_privacy("numeric_histogram", "x", breaks = c(0, 2, 1)),
    "strictly increasing"
  )
  expect_error(
    omop_privacy("numeric_histogram", "x",
                 breaks = c("2020-01-01", "2020-02-30")),
    "invalid"
  )
  expect_error(
    omop_privacy("numeric_histogram", "x", breaks = c(0, 1),
                 reducer = "first"),
    "require order_by"
  )
  expect_identical(
    omop_privacy("numeric_histogram", "x", breaks = c(0, 1),
                 reducer = "first", order_by = "event_date")$order_by,
    "event_date"
  )
  expect_error(
    omop_privacy("categorical_histogram", "x", levels = c("a", "b"),
                 reducer = "mode", max_contributions = 2),
    "one contribution"
  )
  expect_error(
    omop_privacy("bounded_mean", "x", lower = 10, upper = 1),
    "finite positive span"
  )
  expect_error(
    omop_privacy("bounded_mean", "x", lower = 0, upper = 1,
                 max_contributions = 2),
    "one contribution"
  )
  expect_error(omop_privacy("binary_rate", "x"), "positive")
  expect_error(
    omop_privacy("binary_rate", "x", positive = "yes", reducer = "last"),
    "require order_by"
  )
  expect_error(
    omop_privacy("count", population_id = "contains spaces"),
    "population_id"
  )
  expect_error(
    omop_privacy("count", population_id = strrep("a", 257L)),
    "at most 256"
  )
})

test_that("DP status never publishes a partial federation", {
  statuses <- list(a = .dp_status(), b = .dp_status())
  .with_dp_backend(statuses, list(a = NULL, b = NULL),
    function(datasources, sent) {
      value <- ds.omop.dp.status(datasources)
      expect_named(value, c("a", "b"))
      expect_identical(value$a$mechanism,
                       "dsomop-sticky-discrete-laplace-prf-v1")
      expect_identical(value$a$sampler, "hmac-inverse-cdf-52bit-v1")
      expect_false(value$a$sampler_certified)
      expect_false(value$a$formal_dp)
      expect_true(value$a$bounded_accounting)
      expect_false(value$a$bounded_composition)
      expect_identical(
        value$a$privacy_guarantee,
        "sticky_noise_not_formally_certified_dp"
      )
      expect_true(all(vapply(value, `[[`, logical(1L), "sticky_noise")))
    })

  datasources <- list(a = "A", b = "B")
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      if (identical(server, "b")) stop("offline", call. = FALSE)
      stats::setNames(list(.dp_status()), server)
    },
    .package = "DSI"
  )
  expect_error(ds.omop.dp.status(datasources), "no partial result")
})

test_that("DP status requires a valid public snapshot identity", {
  missing_snapshot <- .dp_status()
  missing_snapshot$snapshot_id <- NULL
  .with_dp_backend(list(a = missing_snapshot), list(a = NULL),
    function(datasources, sent) {
      expect_error(ds.omop.dp.status(datasources), "malformed DP status")
    })

  invalid_snapshot <- .dp_status(snapshot_id = "private snapshot")
  .with_dp_backend(list(a = invalid_snapshot), list(a = NULL),
    function(datasources, sent) {
      expect_error(ds.omop.dp.status(datasources), "invalid public snapshot_id")
    })
})

test_that("DP status can inspect a coherently disabled server", {
  disabled <- list(
    enabled = FALSE, ready = FALSE, formal_dp = FALSE,
    sticky_noise = FALSE, durable_ledger = FALSE,
    protocol = "dsomop-dp-release-v1",
    mechanism = "dsomop-sticky-discrete-laplace-prf-v1"
  )
  .with_dp_backend(list(a = disabled), list(a = NULL),
    function(datasources, sent) {
      value <- ds.omop.dp.status(datasources)
      expect_false(value$a$enabled)
      expect_false(value$a$ready)
    })
})

test_that("DP release preflight rejects policy drift before data release", {
  statuses <- list(
    a = .dp_status(),
    b = .dp_status(allocator = "different_allocator")
  )
  releases <- list(a = .dp_release("count", noisy_count = 10),
                   b = .dp_release("count", noisy_count = 20))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"),
                         datasources),
      "allocator.*differs"
    )
    heads <- vapply(sent$expressions, function(expr) as.character(expr[[1L]]),
                    character(1L))
    expect_false("omopDpReleaseDS" %in% heads)
  })
})

test_that("DP release preflight enforces longitudinal statistic contracts", {
  missing <- .dp_status()
  missing$longitudinal_contract <- NULL
  .with_dp_backend(list(a = missing), list(a = NULL),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"),
                           datasources),
        "omitted DP contract"
      )
    })

  drift <- list(a = .dp_status(), b = .dp_status())
  drift$b$longitudinal_contract <- "different_bounding"
  .with_dp_backend(drift, list(a = NULL, b = NULL),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"),
                           datasources),
        "longitudinal_contract.*differs"
      )
    })

  unsupported <- .dp_status()
  unsupported$supported_statistics <- "count"
  .with_dp_backend(list(a = unsupported), list(a = NULL),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release(
          "analysis_table",
          omop_privacy("bounded_mean", "value", lower = 0, upper = 1),
          datasources
        ),
        "does not support"
      )
    })
})

test_that("DP preflight requires one authenticated provenance protocol", {
  missing <- .dp_status()
  missing$person_local_provenance_required <- FALSE
  .with_dp_backend(list(a = missing), list(a = NULL),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"),
                           datasources),
        "incoherent non-blocking"
      )
    })

  statuses <- list(a = .dp_status(), b = .dp_status())
  statuses$b$provenance_protocol <- "different-provenance-contract"
  .with_dp_backend(statuses, list(a = NULL, b = NULL),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"),
                           datasources),
        "provenance_protocol.*differs"
      )
    })
})

test_that("DP release requires formal attestation only when requested", {
  statuses <- list(a = .dp_status())
  releases <- list(a = .dp_release("count", noisy_count = 10))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"),
                         datasources, require_formal = TRUE),
      "does not attest formal DP with bounded composition"
    )
    value <- ds.omop.dp.release(
      "analysis_table", omop_privacy("count"), datasources,
      require_formal = FALSE
    )
    expect_s3_class(value, "dsomop_result")
    expect_null(value$meta$harmonization)
  })
})

test_that("DP status rejects a non-logical formal attestation", {
  status <- .dp_status()
  status$formal_dp <- "FALSE"
  .with_dp_backend(list(a = status), list(a = NULL),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.status(datasources),
        "invalid DP status field 'formal_dp'"
      )
    })
})

test_that("release formal attestation follows the preflight contract", {
  contract <- .dp_status()
  contract$formal_dp <- TRUE
  contract$sampler_certified <- TRUE
  contract$bounded_composition <- TRUE
  release <- .dp_release("count", noisy_count = 10)
  release$formal_dp <- TRUE
  release$sampler_certified <- TRUE
  privacy <- omop_privacy("count")

  expect_identical(
    .dp_release_shape(release, "a", privacy, contract)$formal_dp,
    TRUE
  )
  release$formal_dp <- FALSE
  expect_error(
    .dp_release_shape(release, "a", privacy, contract),
    "'formal_dp'.*preflight contract"
  )
})

test_that("preflight requires the non-blocking accounting contract", {
  statuses <- list(a = .dp_status(never_budget_blocked = FALSE))
  releases <- list(a = .dp_release("count", noisy_count = 10))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
      "incoherent non-blocking"
    )
  })
})

test_that("DP release sends one bare symbol and an encoded typed spec", {
  statuses <- list(a = .dp_status())
  releases <- list(a = .dp_release("count", noisy_count = 10))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources)
    release_call <- Filter(function(expr) {
      identical(as.character(expr[[1L]]), "omopDpReleaseDS")
    }, sent$expressions)[[1L]]
    expect_identical(release_call[[2L]], as.name("analysis_table"))
    expect_true(is.character(release_call[[3L]]))
    expect_match(release_call[[3L]], "^B64:")
    blob <- gsub("_", "/", gsub("-", "+",
      sub("^B64:", "", release_call[[3L]])))
    padding <- (4L - nchar(blob) %% 4L) %% 4L
    if (padding > 0L) blob <- paste0(blob, strrep("=", padding))
    decoded <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(blob)))
    expect_identical(decoded$statistic, "count")
    expect_identical(decoded$population_id, "analysis_table")
    expect_false(any(c("epsilon", "seed", "nonce") %in% names(decoded)))
  })
  expect_error(
    ds.omop.dp.release("x[[1]]", omop_privacy("count"), list(a = "A")),
    "bare DataSHIELD symbol"
  )
})

test_that("explicit population label is canonical and sent unchanged", {
  statuses <- list(a = .dp_status())
  releases <- list(a = .dp_release("count", noisy_count = 10))
  privacy <- omop_privacy(
    "count", population_id = "study/site:adults-v1"
  )
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", privacy, datasources)
    release_call <- Filter(function(expr) {
      identical(as.character(expr[[1L]]), "omopDpReleaseDS")
    }, sent$expressions)[[1L]]
    blob <- gsub("_", "/", gsub("-", "+",
      sub("^B64:", "", release_call[[3L]])))
    padding <- (4L - nchar(blob) %% 4L) %% 4L
    if (padding > 0L) blob <- paste0(blob, strrep("=", padding))
    decoded <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(blob)))
    expect_identical(decoded$population_id, "study/site:adults-v1")
    expect_identical(value$meta$privacy$population_id,
                     "study/site:adults-v1")
  })
})

test_that("DP count and histograms pool only noisy cells", {
  statuses <- list(
    a = .dp_status(snapshot_id = "site-a-snapshot-17"),
    b = .dp_status(snapshot_id = "site-b-snapshot-42")
  )
  count_releases <- list(
    a = .dp_release("count", noisy_count = 10),
    b = .dp_release("count", epsilon = 0.05, noisy_count = 15)
  )
  .with_dp_backend(statuses, count_releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table", omop_privacy("count"), datasources
    )
    expect_identical(value$pooled$noisy_count, 25)
    expect_equal(value$meta$privacy$conservative_epsilon, 0.15)
    expect_identical(value$meta$privacy$per_site_epsilon,
                     c(a = 0.1, b = 0.05))
    expect_identical(value$meta$privacy$composition,
                     "conservative_sequential_across_sites")
    expect_identical(value$meta$privacy$snapshot_id,
                     c(a = "site-a-snapshot-17", b = "site-b-snapshot-42"))
    expect_named(value$meta$privacy$accounting, c("a", "b"))
    expect_identical(value$meta$privacy$accounting$a$accounting_mode,
                     "bounded_accounted")
    expect_true(value$meta$privacy$accounting$a$bounded_accounting)
    expect_false(value$meta$privacy$accounting$a$bounded_composition)
    expect_false(value$meta$privacy$formal_dp)
    expect_identical(value$meta$privacy$sampler,
                     "hmac-inverse-cdf-52bit-v1")
    expect_identical(
      value$meta$privacy$epsilon_semantics,
      "nominal_noise_calibration_not_certified_dp"
    )
    expect_true(any(grepl("not certified as formal differential privacy",
                          value$meta$warnings)))
    expect_true(any(grepl("do not jointly attest disjoint persons",
                          value$meta$warnings)))
    expect_identical(value$meta$privacy$accounting$b$effective_epsilon, 0.05)
    expect_null(value$meta$harmonization)
    heads <- vapply(sent$expressions, function(expr) as.character(expr[[1L]]),
                    character(1L))
    expect_false("omopDisclosureSettingsDS" %in% heads)
  })

  privacy <- omop_privacy(
    "categorical_histogram", "sex", levels = c("F", "M")
  )
  histogram_releases <- list(
    a = .dp_release("categorical_histogram", levels = c("F", "M"),
                    counts = c(4, 6)),
    b = .dp_release("categorical_histogram", levels = c("F", "M"),
                    counts = c(3, 8))
  )
  .with_dp_backend(statuses, histogram_releases,
    function(datasources, sent) {
      value <- ds.omop.dp.release("analysis_table", privacy, datasources)
      expect_identical(value$pooled$level, c("F", "M"))
      expect_identical(value$pooled$noisy_count, c(7, 14))
    })
})

test_that("federated DP release rejects incompatible date semantics", {
  statuses <- list(a = .dp_status(), b = .dp_status())
  privacy <- omop_privacy(
    "categorical_histogram", "sex", levels = c("F", "M")
  )
  releases <- list(
    a = .dp_release("categorical_histogram", levels = c("F", "M"),
                    counts = c(4, 6)),
    b = .dp_release("categorical_histogram", levels = c("F", "M"),
                    counts = c(3, 8))
  )
  disclosures <- list(a = .dp_disclosure(timezone = "UTC"),
                      b = .dp_disclosure(timezone = "Europe/Madrid"))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", privacy, datasources),
      "Federated harmonisation failed"
    )
    heads <- vapply(sent$expressions, function(expr) as.character(expr[[1L]]),
                    character(1L))
    expect_false("omopDpReleaseDS" %in% heads)
  }, disclosures = disclosures)

  .with_dp_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table", privacy, datasources, pool = FALSE
    )
    expect_null(value$pooled)
    expect_null(value$meta$harmonization)
    expect_named(value$per_site, c("a", "b"))
  }, disclosures = disclosures)
})

test_that("bounded means and rates pool noisy sufficient statistics", {
  statuses <- list(a = .dp_status(), b = .dp_status())
  mean_privacy <- omop_privacy(
    "bounded_mean", "value", lower = 0, upper = 200, reducer = "mean"
  )
  mean_releases <- list(
    a = .dp_release("bounded_mean", noisy_count = 10,
                    noisy_sum_grid = 500, lower = 0, upper = 200,
                    numeric_grid = 100),
    b = .dp_release("bounded_mean", noisy_count = 10,
                    noisy_sum_grid = 1000, lower = 0, upper = 200,
                    numeric_grid = 100)
  )
  .with_dp_backend(statuses, mean_releases, function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", mean_privacy, datasources)
    expect_identical(value$pooled$noisy_count, 20)
    expect_identical(value$pooled$noisy_sum_grid, 1500)
    expect_equal(value$pooled$estimate, 150)
  })

  rate_privacy <- omop_privacy("binary_rate", "case", positive = 1L)
  rate_releases <- list(
    a = .dp_release("binary_rate", noisy_numerator = 2,
                    noisy_denominator = 10),
    b = .dp_release("binary_rate", noisy_numerator = 4,
                    noisy_denominator = 10)
  )
  .with_dp_backend(statuses, rate_releases, function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", rate_privacy, datasources)
    expect_equal(value$pooled$estimate, 0.3)
    expect_identical(value$pooled$noisy_numerator, 6)
    expect_identical(value$pooled$noisy_denominator, 20)
  })
})

test_that("temporal histogram releases preserve canonical public breaks", {
  statuses <- list(a = .dp_status())
  privacy <- omop_privacy(
    "numeric_histogram", "event_date",
    breaks = c("2020-01-01", "2020-07-01", "2021-01-01")
  )
  releases <- list(a = .dp_release(
    "numeric_histogram", breaks = privacy$breaks, counts = c(4, 7),
    value_type = "date"
  ))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release("analysis_table", privacy, datasources)
    expect_identical(value$pooled$lower,
                     c("2020-01-01", "2020-07-01"))
    expect_identical(value$pooled$upper,
                     c("2020-07-01", "2021-01-01"))
    expect_identical(value$pooled$noisy_count, c(4, 7))
  })
})

test_that("pooled formats are client-only post-processing of one semantic spec", {
  statuses <- list(a = .dp_status())
  privacy <- omop_privacy(
    "categorical_histogram", "sex", levels = c("M", "F")
  )
  releases <- list(a = .dp_release(
    "categorical_histogram", levels = c("F", "M"), counts = c(4, 7)
  ))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    long <- ds.omop.dp.release(
      "analysis_table", privacy, datasources, format = "long"
    )
    wide <- ds.omop.dp.release(
      "analysis_table", privacy, datasources, format = "wide"
    )
    vector <- ds.omop.dp.release(
      "analysis_table", privacy, datasources, format = "vector"
    )
    raw <- ds.omop.dp.release(
      "analysis_table", privacy, datasources, format = "raw"
    )
    expect_identical(long$pooled$level, c("F", "M"))
    expect_identical(names(wide$pooled), c("F", "M"))
    expect_identical(vector$pooled, structure(
      c(F = 4, M = 7), degraded = FALSE
    ))
    expect_identical(raw$pooled$levels, c("F", "M"))
    expect_identical(raw$pooled$counts, c(4, 7))
    expect_identical(vector$meta$privacy$format, "vector")

    calls <- Filter(function(expr) {
      identical(as.character(expr[[1L]]), "omopDpReleaseDS")
    }, sent$expressions)
    expect_length(calls, 4L)
    specs <- lapply(calls, `[[`, 3L)
    expect_true(all(vapply(specs[-1L], identical, logical(1L), specs[[1L]])))
    blob <- gsub("_", "/", gsub("-", "+", sub("^B64:", "", specs[[1L]])))
    padding <- (4L - nchar(blob) %% 4L) %% 4L
    if (padding > 0L) blob <- paste0(blob, strrep("=", padding))
    decoded <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(blob)))
    expect_false("format" %in% names(decoded))
    expect_identical(decoded$population_id, "analysis_table")
  })
})

test_that("bounded accounting degrades without blocking or leaking data", {
  statuses <- list(a = .dp_status(), b = .dp_status())
  releases <- list(
    a = .dp_release("count", noisy_count = 10),
    b = .dp_release("count", degraded = TRUE, noisy_count = 0)
  )
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table", omop_privacy("count"), datasources
    )
    expect_identical(value$pooled$noisy_count, 10)
    expect_true(value$pooled$degraded)
    expect_true(value$meta$privacy$degraded)
    expect_identical(value$meta$privacy$per_site_degraded,
                     c(a = FALSE, b = TRUE))
    expect_identical(value$meta$privacy$per_site_epsilon,
                     c(a = 0.1, b = 0))
    expect_true(any(grepl("Data-independent degraded", value$meta$warnings)))
  })

  invalid <- list(a = .dp_release(
    "count", degraded = TRUE, noisy_count = 1
  ))
  .with_dp_backend(list(a = .dp_status()), invalid,
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"),
                           datasources),
        "non-constant degraded payload"
      )
    })
})

test_that("sticky_unbounded is labelled as unbounded global composition", {
  statuses <- list(a = .dp_status(accounting_mode = "sticky_unbounded"))
  releases <- list(a = .dp_release(
    "count", noisy_count = 10, accounting_mode = "sticky_unbounded"
  ))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table", omop_privacy("count"), datasources
    )
    expect_false(value$meta$privacy$bounded_composition)
    expect_identical(
      value$meta$privacy$global_composition,
      "unbounded_across_distinct_semantic_queries"
    )
    expect_true(any(grepl("does not provide finite global DP",
                          value$meta$warnings)))
  })
})

test_that("DP release never returns a partial set of site releases", {
  statuses <- list(a = .dp_status(), b = .dp_status())
  releases <- list(
    a = .dp_release("count", noisy_count = 10),
    b = simpleError("simulated release failure")
  )
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
      "no partial result"
    )
  })
})

test_that("DP release rejects private noise state in a server response", {
  statuses <- list(a = .dp_status())
  releases <- list(a = .dp_release(
    "count", noisy_count = 10, seed = "must-never-cross-the-boundary"
  ))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
      "forbidden private noise state"
    )
  })
})

test_that("DP releases enforce a closed statistic-specific schema", {
  status <- list(a = .dp_status())
  extra <- list(a = .dp_release(
    "count", noisy_count = 10, debug_trace = "must-not-cross"
  ))
  .with_dp_backend(status, extra, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"),
                         datasources),
      "malformed DP release schema"
    )
  })

  mean_privacy <- omop_privacy(
    "bounded_mean", "value", lower = 0, upper = 200
  )
  incoherent_mean <- list(a = .dp_release(
    "bounded_mean", noisy_count = 10, noisy_sum_grid = 500,
    lower = 0, upper = 200, numeric_grid = 100, value = 199
  ))
  .with_dp_backend(status, incoherent_mean, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", mean_privacy, datasources),
      "incoherent bounded-mean estimate"
    )
  })

  mixed_types <- list(
    a = .dp_release(
      "bounded_mean", noisy_count = 10, noisy_sum_grid = 500,
      lower = 0, upper = 200, numeric_grid = 100
    ),
    b = .dp_release(
      "bounded_mean", noisy_count = 10, noisy_sum_grid = 500,
      lower = 0, upper = 200, numeric_grid = 100, value_type = "date"
    )
  )
  .with_dp_backend(list(a = .dp_status(), b = .dp_status()), mixed_types,
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", mean_privacy, datasources),
        "value_type"
      )
    })

  wrong_mean_type <- list(a = .dp_release(
    "bounded_mean", noisy_count = 10, noisy_sum_grid = 500,
    lower = 0, upper = 200, numeric_grid = 100, value_type = "date"
  ))
  .with_dp_backend(status, wrong_mean_type, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", mean_privacy, datasources),
      "value_type"
    )
  })

  wrong_sensitivity <- .dp_release("count", noisy_count = 10)
  wrong_sensitivity$sensitivity$l1 <- 2
  .with_dp_backend(status, list(a = wrong_sensitivity),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"),
                           datasources),
        "sensitivity.l1"
      )
    })
})

test_that("effective epsilon is validated against the server maximum", {
  statuses <- list(a = .dp_status(epsilon = 0.1))
  releases <- list(a = .dp_release(
    "count", epsilon = 0.2, noisy_count = 10
  ))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
      "invalid effective DP allocation"
    )
  })
})

test_that("parallel epsilon is used only with explicit all-site attestation", {
  statuses <- list(
    a = .dp_status(disjoint_persons = TRUE),
    b = .dp_status(disjoint_persons = TRUE)
  )
  releases <- list(a = .dp_release("count", noisy_count = 10),
                   b = .dp_release("count", noisy_count = 20))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table", omop_privacy("count"), datasources
    )
    expect_identical(value$meta$privacy$conservative_epsilon, 0.1)
    expect_identical(value$meta$privacy$composition,
                     "parallel_disjoint_persons")
    expect_false(any(grepl("do not jointly attest disjoint persons",
                           value$meta$warnings)))
  })
})
