.dp_status <- function(epsilon = 0.1,
                       snapshot_id = "snapshot-2026-08-01",
                       noise_domain_id = NULL,
                       domain = NULL) {
  value <- list(
    enabled = TRUE,
    ready = TRUE,
    sticky_noise = TRUE,
    protocol = .DP_PROTOCOL,
    canonical_protocol = "dsomop-dp-canonical-json-v1",
    mechanism = "dsomop-sticky-discrete-laplace-prf-v1",
    sampler = "hmac-inverse-cdf-52bit-v1",
    privacy_guarantee = .DP_PRIVACY_GUARANTEE,
    privacy_contract = .DP_PRIVACY_CONTRACT,
    person_local_provenance_required = TRUE,
    provenance_protocol = "dsomop-dp-person-local-provenance-v2",
    adjacency = "add_remove_person",
    domain = domain,
    snapshot_id = snapshot_id,
    release_epsilon = epsilon,
    release_delta = 0,
    supported_statistics = c(
      "count", "bounded_record_count", "categorical_histogram",
      "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate"
    ),
    longitudinal_contract = "deterministic_person_bounding_v1",
    privacy_epoch = 1L,
    max_levels = 100L,
    max_contributions = 10L,
    numeric_grid = 100L,
    history_dependent = TRUE,
    privacy_call_quota = "none",
    persistent_state = "noise_root_and_release_bindings",
    release_binding = .DP_RELEASE_BINDING,
    service_capacity = .DP_SERVICE_CAPACITY
  )
  if (!is.null(noise_domain_id)) value$noise_domain_id <- noise_domain_id
  value
}

.dp_release <- function(statistic, epsilon = 0.1, ...) {
  supplied <- list(...)
  defaults <- switch(
    statistic,
    count = list(),
    bounded_record_count = list(
      reducer = "records", max_contributions = 1L
    ),
    categorical_histogram = list(
      reducer = "presence", max_contributions = 1L,
      value_type = "categorical_utf8_v1"
    ),
    numeric_histogram = list(
      reducer = "mean", max_contributions = 1L, value_type = "number",
      interval_contract = "left_closed_right_open_last_closed"
    ),
    bounded_distinct = list(
      reducer = "distinct", max_contributions = 1L,
      domain_size = length(supplied$levels),
      selection_order = "canonical_utf8_value_radix",
      value_type = "categorical_utf8_v1"
    ),
    bounded_mean = list(reducer = "mean", value_type = "number"),
    binary_rate = list(
      reducer = "any", denominator = "all_persons",
      value_type = "categorical_utf8_v1"
    )
  )
  payload <- utils::modifyList(defaults, supplied)
  if (identical(statistic, "bounded_distinct")) payload$levels <- NULL
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
    bounded_record_count = list(
      l1 = payload$max_contributions, unit = "person"
    ),
    categorical_histogram = list(
      l1 = payload$max_contributions, unit = "person"
    ),
    numeric_histogram = list(
      l1 = payload$max_contributions, unit = "person"
    ),
    bounded_distinct = list(
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
    protocol = .DP_PROTOCOL,
    mechanism = "dsomop-sticky-discrete-laplace-prf-v1",
    adjacency = "add_remove_person",
    epsilon = epsilon,
    delta = 0,
    privacy_contract = .DP_PRIVACY_CONTRACT,
    sticky = TRUE,
    sampler = "hmac-inverse-cdf-52bit-v1",
    sensitivity = sensitivity,
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
    max_events_per_group = 100,
    max_filter_depth = 32,
    max_filter_nodes = 1024,
    max_filter_values = 10000,
    max_plan_outputs = 100,
    max_analysis_scope_tables = 8
  )
}

.with_dp_backend <- function(statuses, releases, code, disclosures = NULL) {
  for (index in seq_along(statuses)) {
    digit <- c(as.character(0:9), letters[1:6])[[index + 1L]]
    defaults <- list(
      noise_domain_id = paste0("dpn_", strrep(digit, 40L)),
      domain = paste0("dsomop-dp-test-", digit)
    )
    for (field in names(defaults)) {
      if (is.null(statuses[[index]][[field]])) {
        statuses[[index]][[field]] <- defaults[[field]]
      }
    }
  }
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

  record_count <- omop_privacy(
    "bounded_record_count", max_contributions = 3L
  )
  expect_identical(record_count$reducer, "records")
  expect_identical(record_count$max_contributions, 3L)

  categorical_records <- omop_privacy(
    "categorical_histogram", "category", levels = c("b", "a"),
    reducer = "records", max_contributions = 3L, order_by = "event_date"
  )
  expect_identical(categorical_records$levels, c("a", "b"))
  expect_identical(categorical_records$order_by, "event_date")

  distinct <- omop_privacy(
    "bounded_distinct", "concept_id", levels = c(20L, 10L, 20L),
    max_contributions = 2L
  )
  expect_identical(distinct$levels, c("10", "20"))
  expect_identical(distinct$reducer, "distinct")

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
    omop_privacy(
      "categorical_histogram", "x", levels = c("a", "b"),
      reducer = "records", max_contributions = 2
    ),
    "require order_by"
  )
  expect_error(
    omop_privacy("bounded_record_count", variable = "x"),
    "not valid"
  )
  expect_error(
    omop_privacy("bounded_distinct", "concept_id", max_contributions = 2),
    "levels"
  )
  expect_error(
    omop_privacy(
      "bounded_distinct", "concept_id", levels = c(1, 2),
      max_contributions = 2, order_by = "event_date"
    ),
    "not valid"
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
      expect_identical(value$a$protocol, .DP_PROTOCOL)
      expect_identical(value$a$privacy_contract, .DP_PRIVACY_CONTRACT)
      expect_true(value$a$history_dependent)
      expect_identical(value$a$privacy_call_quota, "none")
      expect_identical(
        value$a$privacy_guarantee,
        .DP_PRIVACY_GUARANTEE
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
    enabled = FALSE, ready = FALSE, sticky_noise = FALSE,
    protocol = .DP_PROTOCOL,
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
  statuses <- list(a = .dp_status(), b = .dp_status())
  statuses$b$canonical_protocol <- "different-canonical-protocol"
  releases <- list(a = .dp_release("count", noisy_count = 10),
                   b = .dp_release("count", noisy_count = 20))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"),
                         datasources),
      "canonical_protocol.*differs"
    )
    heads <- vapply(sent$expressions, function(expr) as.character(expr[[1L]]),
                    character(1L))
    expect_false("omopDpReleaseDS" %in% heads)
  })
})

test_that("DP release refuses duplicate logical noise domains", {
  shared <- paste0("dpn_", strrep("a", 40L))
  statuses <- list(
    a = .dp_status(noise_domain_id = shared),
    b = .dp_status(noise_domain_id = shared)
  )
  releases <- list(a = .dp_release("count", noisy_count = 10),
                   b = .dp_release("count", noisy_count = 20))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"),
                         datasources),
      "share a DP noise domain"
    )
    heads <- vapply(sent$expressions, function(expr) as.character(expr[[1L]]),
                    character(1L))
    expect_false("omopDpReleaseDS" %in% heads)
  })
})

test_that("DP release refuses one logical domain with different roots", {
  statuses <- list(
    a = .dp_status(domain = "shared-logical-node"),
    b = .dp_status(domain = "shared-logical-node")
  )
  releases <- list(a = .dp_release("count", noisy_count = 10),
                   b = .dp_release("count", noisy_count = 20))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"),
                         datasources),
      "share a DP logical domain"
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
        "incoherent first-answer"
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

test_that("DP release API exposes no analyst privacy controls", {
  statuses <- list(a = .dp_status())
  releases <- list(a = .dp_release("count", noisy_count = 10))
  expect_identical(
    names(formals(ds.omop.dp.release)),
    c("x", "privacy", "datasources", "pool", "format", "type")
  )
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table", omop_privacy("count"), datasources
    )
    expect_s3_class(value, "dsomop_result")
    expect_null(value$meta$harmonization)
  })
})

test_that("DP release supports split, combine, and both result views", {
  statuses <- list(a = .dp_status(), b = .dp_status())
  releases <- list(
    a = .dp_release("count", noisy_count = 10),
    b = .dp_release("count", noisy_count = 15)
  )
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    cases <- list(s = c(2L, 0L), combined = c(0L, 1L), b = c(2L, 1L))
    for (alias in names(cases)) {
      value <- ds.omop.dp.release(
        "analysis_table", omop_privacy("count"), datasources, type = alias
      )
      expect_s3_class(value, "dsomop_result")
      expect_length(value$per_site, cases[[alias]][[1L]])
      expect_identical(as.integer(!is.null(value$pooled)),
                       cases[[alias]][[2L]])
    }
  })
})

test_that("v1 status is rejected before requesting a release", {
  status <- .dp_status()
  status$protocol <- "dsomop-dp-release-v1"
  .with_dp_backend(list(a = status), list(a = NULL),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"),
                           datasources),
        "unsupported DP release contract"
      )
      heads <- vapply(sent$expressions, function(expr) as.character(expr[[1L]]),
                      character(1L))
      expect_false("omopDpReleaseDS" %in% heads)
    })
})

test_that("release schema rejects unexpected fields", {
  contract <- .dp_status()
  release <- .dp_release("count", noisy_count = 10)
  release$unexpected <- TRUE
  privacy <- omop_privacy("count")

  expect_error(
    .dp_release_shape(release, "a", privacy, contract),
    "malformed DP release schema"
  )
})

test_that("preflight requires the implemented release contract", {
  for (field in c("privacy_guarantee", "privacy_contract")) {
    status <- .dp_status()
    status[[field]] <- "unknown-contract"
    .with_dp_backend(list(a = status), list(a = NULL),
      function(datasources, sent) {
        expect_error(
          ds.omop.dp.release("analysis_table", omop_privacy("count"),
                             datasources),
          "unsupported DP release contract"
        )
      })
  }
})

test_that("preflight requires first-answer state with no privacy call quota", {
  status <- .dp_status()
  status$privacy_call_quota <- "daily"
  statuses <- list(a = status)
  releases <- list(a = .dp_release("count", noisy_count = 10))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
      "incoherent first-answer"
    )
  })

  status <- .dp_status()
  status$history_dependent <- FALSE
  .with_dp_backend(list(a = status), releases, function(datasources, sent) {
    expect_error(
      ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
      "incoherent first-answer"
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
    b = .dp_status(epsilon = 0.05, snapshot_id = "site-b-snapshot-42")
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
    expect_equal(value$meta$privacy$conservative_epsilon, 0.1)
    expect_identical(value$meta$privacy$per_site_epsilon,
                     c(a = 0.1, b = 0.05))
    expect_identical(value$meta$privacy$composition,
                     "parallel_across_sites")
    expect_identical(value$meta$privacy$snapshot_id,
                     c(a = "site-a-snapshot-17", b = "site-b-snapshot-42"))
    expect_identical(value$meta$privacy$privacy_contract,
                     .DP_PRIVACY_CONTRACT)
    expect_identical(value$meta$privacy$composition_scope,
                     "current_federated_release")
    expect_identical(value$meta$privacy$privacy_call_quota, "none")
    expect_true(value$meta$privacy$history_dependent)
    expect_identical(value$meta$privacy$persistent_state,
                     "noise_root_and_release_bindings")
    expect_identical(value$meta$privacy$release_binding, .DP_RELEASE_BINDING)
    expect_identical(value$meta$privacy$service_capacity, .DP_SERVICE_CAPACITY)
    expect_identical(value$meta$privacy$sampler,
                     "hmac-inverse-cdf-52bit-v1")
    expect_identical(
      value$meta$privacy$privacy_guarantee,
      .DP_PRIVACY_GUARANTEE
    )
    expect_length(value$meta$warnings, 0L)
    expect_identical(value$meta$privacy$per_site_epsilon[["b"]], 0.05)
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
    b = .dp_release("categorical_histogram", epsilon = 0.05,
                    levels = c("F", "M"), counts = c(3, 8))
  )
  .with_dp_backend(statuses, histogram_releases,
    function(datasources, sent) {
      value <- ds.omop.dp.release("analysis_table", privacy, datasources)
      expect_identical(value$pooled$level, c("F", "M"))
      expect_identical(value$pooled$noisy_count, c(7, 14))
    })
})

test_that("bounded record and distinct counts pool scalar noisy releases", {
  statuses <- list(a = .dp_status(), b = .dp_status())

  record_privacy <- omop_privacy(
    "bounded_record_count", max_contributions = 3L
  )
  record_releases <- list(
    a = .dp_release(
      "bounded_record_count", noisy_count = 7,
      max_contributions = 3L
    ),
    b = .dp_release(
      "bounded_record_count", noisy_count = 5,
      max_contributions = 3L
    )
  )
  .with_dp_backend(statuses, record_releases,
    function(datasources, sent) {
      value <- ds.omop.dp.release(
        "analysis_table", record_privacy, datasources
      )
      expect_identical(value$pooled$noisy_count, 12)
      heads <- vapply(
        sent$expressions, function(expr) as.character(expr[[1L]]),
        character(1L)
      )
      expect_false("omopDisclosureSettingsDS" %in% heads)
    })

  distinct_privacy <- omop_privacy(
    "bounded_distinct", "concept_id", levels = c(10L, 20L, 30L),
    max_contributions = 2L
  )
  distinct_releases <- list(
    a = .dp_release(
      "bounded_distinct", levels = distinct_privacy$levels,
      noisy_count = 2, max_contributions = 2L
    ),
    b = .dp_release(
      "bounded_distinct", levels = distinct_privacy$levels,
      noisy_count = 3, max_contributions = 2L
    )
  )
  .with_dp_backend(statuses, distinct_releases,
    function(datasources, sent) {
      value <- ds.omop.dp.release(
        "analysis_table", distinct_privacy, datasources
      )
      expect_identical(value$pooled$noisy_count, 5)
      expect_identical(
        value$pooled$pooling,
        "sum_of_site_local_distinct_cardinalities"
      )
      expect_identical(value$per_site$a$domain_size, 3L)
      expect_true(any(grepl(
        "not the cardinality of the cross-site concept union",
        value$meta$warnings, fixed = TRUE
      )))
      heads <- vapply(
        sent$expressions, function(expr) as.character(expr[[1L]]),
        character(1L)
      )
      expect_false("omopDisclosureSettingsDS" %in% heads)
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
    expect_identical(vector$pooled, c(F = 4, M = 7))
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

test_that("repeated releases have no client-side usage state", {
  statuses <- list(a = .dp_status())
  releases <- list(a = .dp_release("count", noisy_count = 10))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    values <- replicate(25L, ds.omop.dp.release(
      "analysis_table", omop_privacy("count"), datasources
    ), simplify = FALSE)
    expect_true(all(vapply(values, function(value) {
      identical(value$pooled$noisy_count, 10) &&
        identical(value$meta$privacy$privacy_call_quota, "none") &&
        identical(value$meta$privacy$composition_scope,
                  "current_federated_release")
    }, logical(1L))))
    calls <- Filter(function(expr) {
      identical(as.character(expr[[1L]]), "omopDpReleaseDS")
    }, sent$expressions)
    expect_length(calls, 25L)
    specs <- lapply(calls, `[[`, 3L)
    expect_true(all(vapply(specs[-1L], identical, logical(1L), specs[[1L]])))
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

test_that("release epsilon must equal the server's fixed value", {
  for (epsilon in c(0.05, 0.2)) {
    statuses <- list(a = .dp_status(epsilon = 0.1))
    releases <- list(a = .dp_release(
      "count", epsilon = epsilon, noisy_count = 10
    ))
    .with_dp_backend(statuses, releases, function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
        "fixed per-release contract"
      )
    })
  }
})

test_that("status and release require delta zero", {
  status <- .dp_status()
  status$release_delta <- 1e-6
  .with_dp_backend(list(a = status), list(a = NULL),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
        "out-of-range DP contract"
      )
    })

  release <- .dp_release("count", noisy_count = 10)
  release$delta <- 1e-6
  .with_dp_backend(list(a = .dp_status()), list(a = release),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
        "fixed per-release contract"
      )
    })
})

test_that("federated releases always use parallel cross-site composition", {
  statuses <- list(
    a = .dp_status(epsilon = 0.1),
    b = .dp_status(epsilon = 0.05)
  )
  releases <- list(a = .dp_release("count", noisy_count = 10),
                   b = .dp_release("count", epsilon = 0.05, noisy_count = 20))
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    value <- ds.omop.dp.release(
      "analysis_table", omop_privacy("count"), datasources
    )
    expect_identical(value$meta$privacy$conservative_epsilon, 0.1)
    expect_identical(value$meta$privacy$composition,
                     "parallel_across_sites")
    expect_length(value$meta$warnings, 0L)
  })
})


test_that("v3 status validates the complete first-answer tuple", {
  fields <- c("history_dependent", "persistent_state", "release_binding",
              "privacy_call_quota", "service_capacity")
  for (field in fields) {
    for (invalid in list(NULL, NA, character(), c("a", "b"), "unknown", 1)) {
      status <- .dp_status()
      status[field] <- list(invalid)
      .with_dp_backend(list(a = status), list(a = NULL),
        function(datasources, sent) {
          expect_error(ds.omop.dp.status(datasources),
                       paste0("incoherent first-answer.*", field))
          expect_error(
            ds.omop.dp.release("analysis_table", omop_privacy("count"),
                               datasources),
            paste0("incoherent first-answer.*", field)
          )
          heads <- vapply(sent$expressions, function(expr) {
            as.character(expr[[1L]])
          }, character(1L))
          expect_false("omopDpReleaseDS" %in% heads)
        })
    }
    status <- .dp_status()
    status[[field]] <- NULL
    .with_dp_backend(list(a = status), list(a = NULL),
      function(datasources, sent) {
        expect_error(ds.omop.dp.status(datasources),
                     paste0("omitted DP contract.*", field))
      })
  }
})

test_that("legacy contracts remain inspectable but cannot release or pool", {
  legacy <- .dp_status()
  legacy$protocol <- .DP_LEGACY_PROTOCOL
  legacy$privacy_contract <- .DP_LEGACY_PRIVACY_CONTRACT
  legacy$history_dependent <- FALSE
  legacy$persistent_state <- "noise_root_only"
  legacy$release_binding <- NULL
  legacy$service_capacity <- NULL
  for (statuses in list(list(old = legacy),
                        list(new = .dp_status(), old = legacy))) {
    .with_dp_backend(statuses, rep(list(NULL), length(statuses)),
      function(datasources, sent) {
        inspected <- ds.omop.dp.status(datasources)
        expect_identical(inspected$old$protocol, .DP_LEGACY_PROTOCOL)
        expect_false(inspected$old$history_dependent)
        printed <- paste(capture.output(print(inspected)), collapse = "\n")
        expect_match(printed, "legacy_v2 (no first-answer binding)",
                     fixed = TRUE)
        expect_error(
          ds.omop.dp.release("analysis_table", omop_privacy("count"),
                             datasources),
          "legacy v2.*Upgrade to dsOMOP 2.7.1.*cannot be pooled"
        )
        heads <- vapply(sent$expressions, function(expr) {
          as.character(expr[[1L]])
        }, character(1L))
        expect_false("omopDpReleaseDS" %in% heads)
      })
  }
})

test_that("protocol and privacy contract cannot be combined across versions", {
  status <- .dp_status()
  status$privacy_contract <- .DP_LEGACY_PRIVACY_CONTRACT
  .with_dp_backend(list(a = status), list(a = NULL),
    function(datasources, sent) {
      expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
        "unsupported DP release contract"
      )
      heads <- vapply(sent$expressions, function(expr) as.character(expr[[1L]]),
                      character(1L))
      expect_false("omopDpReleaseDS" %in% heads)
    })
})

.dp_legacy_status <- function(...) {
  status <- .dp_status(...)
  status$protocol <- .DP_LEGACY_PROTOCOL
  status$privacy_contract <- .DP_LEGACY_PRIVACY_CONTRACT
  status$history_dependent <- FALSE
  status$persistent_state <- "noise_root_only"
  status$release_binding <- NULL
  status$service_capacity <- NULL
  status
}

.dp_as_legacy_release <- function(release) {
  release$protocol <- .DP_LEGACY_PROTOCOL
  release$privacy_contract <- .DP_LEGACY_PRIVACY_CONTRACT
  release
}

test_that("legacy release opt-in requires the literal TRUE option", {
  for (setting in list(NULL, FALSE, NA, 1, "TRUE", c(TRUE, TRUE))) {
    withr::local_options(list(dsomop.dp.allow_legacy_servers = setting))
    .with_dp_backend(list(old = .dp_legacy_status()), list(old = NULL),
      function(datasources, sent) {
        expect_error(
          ds.omop.dp.release("analysis_table", omop_privacy("count"),
                             datasources),
          "legacy v2.*Upgrade to dsOMOP 2.7.1.*cannot be pooled"
        )
        heads <- vapply(sent$expressions, function(expr) as.character(expr[[1L]]),
                        character(1L))
        expect_false("omopDpReleaseDS" %in% heads)
      })
  }
})

test_that("legacy opt-in warns for every old site and retains its contract", {
  withr::local_options(dsomop.dp.allow_legacy_servers = TRUE)
  statuses <- list(old_a = .dp_legacy_status(), old_b = .dp_legacy_status())
  releases <- list(
    old_a = .dp_as_legacy_release(.dp_release("count", noisy_count = 10)),
    old_b = .dp_as_legacy_release(.dp_release("count", noisy_count = 20))
  )
  .with_dp_backend(statuses, releases, function(datasources, sent) {
    warnings <- character()
    value <- withCallingHandlers(
      ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(warnings, 2L)
    residual <- paste0(
      "no first-answer binding: an unrotated data refresh can reveal whether ",
      "a released statistic changed; see isglobal-brge/dsOMOP#20"
    )
    for (index in seq_along(statuses)) {
      expect_match(warnings[[index]], names(statuses)[[index]], fixed = TRUE)
      expect_match(warnings[[index]], residual, fixed = TRUE)
    }
    expect_identical(value$meta$warnings, warnings)
    expect_identical(value$pooled$noisy_count, 30)
    expect_identical(value$meta$privacy$legacy_servers, names(statuses))
    expect_false(value$meta$privacy$mixed_contracts)
    expect_identical(value$meta$privacy$privacy_contract,
                     .DP_LEGACY_PRIVACY_CONTRACT)
    expect_false(value$meta$privacy$history_dependent)
    expect_identical(value$meta$privacy$persistent_state, "noise_root_only")
    expect_null(value$meta$privacy$release_binding)
    for (server in names(statuses)) {
      site <- value$meta$privacy$per_site_contract[[server]]
      expect_identical(site$protocol, .DP_LEGACY_PROTOCOL)
      expect_identical(site$privacy_contract, .DP_LEGACY_PRIVACY_CONTRACT)
      expect_false(site$history_dependent)
      expect_null(site$release_binding)
    }
  })
})

test_that("mixed releases record each contract in every result view and order", {
  withr::local_options(dsomop.dp.allow_legacy_servers = TRUE)
  statuses <- list(new = .dp_status(), old = .dp_legacy_status())
  releases <- list(
    new = .dp_release("count", noisy_count = 10),
    old = .dp_as_legacy_release(.dp_release("count", noisy_count = 20))
  )
  for (order in list(c("new", "old"), c("old", "new"))) {
    for (type in c("split", "combine", "both")) {
      .with_dp_backend(statuses[order], releases[order],
        function(datasources, sent) {
          expect_warning(value <- ds.omop.dp.release(
            "analysis_table", omop_privacy("count"), datasources, type = type
          ), "Legacy DP server 'old': no first-answer binding")
          expect_true(value$meta$privacy$mixed_contracts)
          expect_identical(value$meta$privacy$legacy_servers, "old")
          expect_length(value$meta$warnings, 1L)
          expect_match(value$meta$warnings[[1L]], "isglobal-brge/dsOMOP#20",
                       fixed = TRUE)
          contracts <- value$meta$privacy$per_site_contract
          expect_named(contracts, order)
          expect_identical(contracts$new$protocol, .DP_PROTOCOL)
          expect_identical(contracts$new$privacy_contract, .DP_PRIVACY_CONTRACT)
          expect_identical(contracts$new$release_binding, .DP_RELEASE_BINDING)
          expect_true(contracts$new$history_dependent)
          expect_identical(contracts$old$protocol, .DP_LEGACY_PROTOCOL)
          expect_identical(contracts$old$privacy_contract,
                           .DP_LEGACY_PRIVACY_CONTRACT)
          expect_false(contracts$old$history_dependent)
          expect_null(contracts$old$release_binding)
          for (field in .DP_VERSIONED_CONTRACT_FIELDS) {
            expect_null(value$meta$privacy[[field]])
          }
          if (type != "split") expect_identical(value$pooled$noisy_count, 30)
          if (type == "combine") expect_identical(value$per_site, list())
        })
    }
  }
})

test_that("legacy opt-in retains the exact old contract and common checks", {
  withr::local_options(dsomop.dp.allow_legacy_servers = TRUE)
  mutations <- list(
    history_dependent = TRUE,
    privacy_call_quota = "daily",
    persistent_state = "noise_root_and_release_bindings",
    release_binding = .DP_RELEASE_BINDING,
    service_capacity = .DP_SERVICE_CAPACITY,
    person_local_provenance_required = FALSE,
    privacy_guarantee = "unknown",
    privacy_contract = .DP_PRIVACY_CONTRACT,
    release_delta = 1e-6,
    release_epsilon = 0,
    sampler = "unknown",
    mechanism = "unknown",
    canonical_protocol = "unknown",
    provenance_protocol = "unknown",
    longitudinal_contract = "unknown",
    adjacency = "unknown"
  )
  for (field in names(mutations)) {
    status <- .dp_legacy_status()
    status[[field]] <- mutations[[field]]
    .with_dp_backend(list(new = .dp_status(), old = status),
      list(new = NULL, old = NULL), function(datasources, sent) {
        expect_error(
          ds.omop.dp.release("analysis_table", omop_privacy("count"),
                             datasources),
          "DP.*contract|DP.*provenance"
        )
        heads <- vapply(sent$expressions, function(expr) as.character(expr[[1L]]),
                        character(1L))
        expect_false("omopDpReleaseDS" %in% heads)
      })
  }
  for (field in c("history_dependent", "privacy_call_quota", "persistent_state",
                  "person_local_provenance_required")) {
    status <- .dp_legacy_status()
    status[[field]] <- NULL
    .with_dp_backend(list(old = status), list(old = NULL),
      function(datasources, sent) {
        expect_error(
          ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
          paste0("omitted DP contract.*", field)
        )
      })
  }
  for (field in c("domain", "noise_domain_id")) {
    new <- .dp_status(noise_domain_id = paste0("dpn_", strrep("a", 40L)),
                      domain = "site-a")
    old <- .dp_legacy_status()
    old[[field]] <- new[[field]]
    .with_dp_backend(list(new = new, old = old), list(new = NULL, old = NULL),
      function(datasources, sent) {
        expect_error(
          ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
          "share a DP.*domain"
        )
      })
  }
})

test_that("legacy opt-in rejects cross-version payloads after preflight", {
  withr::local_options(dsomop.dp.allow_legacy_servers = TRUE)
  modern_release <- .dp_release("count", noisy_count = 10)
  old_release <- .dp_as_legacy_release(modern_release)
  for (legacy_first in c(TRUE, FALSE)) {
    statuses <- if (legacy_first) {
      list(a = .dp_legacy_status(), b = .dp_status())
    } else list(a = .dp_status(), b = .dp_legacy_status())
    releases <- if (legacy_first) {
      list(a = modern_release, b = modern_release)
    } else list(a = old_release, b = old_release)
    .with_dp_backend(statuses, releases, function(datasources, sent) {
      expect_warning(expect_error(
        ds.omop.dp.release("analysis_table", omop_privacy("count"), datasources),
        "protocol.*does not match the preflight contract"
      ), "no first-answer binding")
    })
  }
})

test_that("all seven primitives keep pooling unchanged during mixed transition", {
  withr::local_options(dsomop.dp.allow_legacy_servers = TRUE)
  cases <- list(
    list(omop_privacy("count"), .dp_release("count", noisy_count = 10)),
    list(omop_privacy("bounded_record_count"),
         .dp_release("bounded_record_count", noisy_count = 10)),
    list(omop_privacy("categorical_histogram", "sex", levels = c("F", "M")),
         .dp_release("categorical_histogram", levels = c("F", "M"),
                     counts = c(4, 6))),
    list(omop_privacy("numeric_histogram", "age", breaks = c(0, 50, 100)),
         .dp_release("numeric_histogram", breaks = c(0, 50, 100),
                     counts = c(4, 6))),
    list(omop_privacy("bounded_distinct", "concept", levels = c("a", "b")),
         .dp_release("bounded_distinct", levels = c("a", "b"), noisy_count = 2)),
    list(omop_privacy("bounded_mean", "value", lower = 0, upper = 200),
         .dp_release("bounded_mean", noisy_count = 10, noisy_sum_grid = 500,
                     lower = 0, upper = 200, numeric_grid = 100)),
    list(omop_privacy("binary_rate", "case", positive = 1L),
         .dp_release("binary_rate", noisy_numerator = 2, noisy_denominator = 10))
  )
  for (case in cases) {
    reference <- NULL
    .with_dp_backend(list(a = .dp_status(), b = .dp_status()),
      list(a = case[[2L]], b = case[[2L]]), function(datasources, sent) {
        reference <<- ds.omop.dp.release("analysis_table", case[[1L]], datasources)
      })
    .with_dp_backend(list(a = .dp_status(), b = .dp_legacy_status()),
      list(a = case[[2L]], b = .dp_as_legacy_release(case[[2L]])),
      function(datasources, sent) {
        expect_warning(value <- ds.omop.dp.release(
          "analysis_table", case[[1L]], datasources
        ), "Legacy DP server 'b'")
        expect_identical(value$pooled, reference$pooled)
        expect_identical(value$meta$harmonization, reference$meta$harmonization)
      })
  }
})
