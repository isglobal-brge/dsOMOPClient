# Typed analysis-catalog pooling: the client consumes only server-owned
# semantics and never guesses from column names.

.pc_col <- function(role, ...) c(list(role = role), list(...))

test_that("incidence counts and rates pool from contracted sufficient statistics", {
  contract <- list(
    version = 1L, strategy = "tabular",
    columns = list(
      stratum = .pc_col("key"),
      persons_at_risk = .pc_col("sum"),
      person_days = .pc_col("sum"),
      outcomes = .pc_col("sum"),
      person_outcomes = .pc_col("sum"),
      proportion = .pc_col("ratio", numerator = "person_outcomes",
                           denominator = "persons_at_risk", scale = 1),
      rate = .pc_col("ratio", numerator = "person_outcomes",
                     denominator = "person_days", scale = 1)
    )
  )
  a <- data.frame(
    stratum = c("common", "local"), persons_at_risk = c(100, 20),
    person_days = c(1000, 100), outcomes = c(15, 5),
    person_outcomes = c(10, 5), proportion = c(.1, .25),
    rate = c(.01, .05), stringsAsFactors = FALSE
  )
  b <- data.frame(
    stratum = "common", persons_at_risk = 50, person_days = 500,
    outcomes = 8, person_outcomes = 5, proportion = .1, rate = .01,
    stringsAsFactors = FALSE
  )
  out <- dsOMOPClient:::.pool_analysis_contract(
    list(a = a, b = b), contract, policy = "strict"
  )
  expect_identical(names(out$result), names(contract$columns))
  expect_equal(out$result$stratum, "common")
  expect_equal(out$result$persons_at_risk, 150)
  expect_equal(out$result$person_days, 1500)
  expect_equal(out$result$person_outcomes, 15)
  expect_equal(out$result$proportion, 15 / 150)
  expect_equal(out$result$rate, 15 / 1500)
  expect_true(any(grepl("absent or suppressed", out$warnings)))
  expect_true(any(grepl("multiple databases", out$warnings)))
})

test_that("vector ratio components support exact derived epidemiologic measures", {
  contract <- list(
    version = 1L, strategy = "tabular",
    columns = list(
      threshold = .pc_col("key"), tp = .pc_col("sum"),
      fp = .pc_col("sum"),
      ppv = .pc_col("ratio", numerator = "tp",
                    denominator = c("tp", "fp"), scale = 1)
    )
  )
  frame <- function(tp, fp) data.frame(
    threshold = .5, tp = tp, fp = fp, ppv = NA_real_
  )
  out <- dsOMOPClient:::.pool_analysis_contract(
    list(a = frame(30, 10), b = frame(20, 20)), contract
  )
  expect_equal(out$result$tp, 50)
  expect_equal(out$result$fp, 30)
  expect_equal(out$result$ppv, 50 / 80)
})

test_that("OHDSI identifiers pool independently of presentation labels", {
  contract <- list(
    version = 1L, strategy = "tabular",
    columns = list(
      concept_id = .pc_col("key"),
      concept_name = .pc_col("label"),
      n_persons = .pc_col("sum")
    )
  )
  out <- dsOMOPClient:::.pool_analysis_contract(list(
    a = data.frame(concept_id = 111L, concept_name = "Label A", n_persons = 20),
    b = data.frame(concept_id = 111L, concept_name = "Label B", n_persons = 30)
  ), contract)
  expect_equal(out$result$concept_id, 111L)
  expect_equal(out$result$n_persons, 50)
  expect_true(is.na(out$result$concept_name))
  expect_true(any(grepl("Conflicting presentation labels", out$warnings)))
})

test_that("treatment-path distributions use weighted moments and retain shape", {
  contract <- list(
    version = 1L, strategy = "tabular",
    columns = list(
      covariate_id = .pc_col("key"),
      count_value = .pc_col("sum"),
      avg_value = .pc_col("weighted_mean", weight = "count_value"),
      stdev_value = .pc_col("pooled_sd", mean = "avg_value",
                            count = "count_value"),
      median_value = .pc_col("nonpoolable", reason = "No pooled quantiles.")
    )
  )
  a <- data.frame(covariate_id = c(1L, 2L), count_value = c(20, 10),
                  avg_value = c(2, 8), stdev_value = c(1, 1),
                  median_value = c(2, 8))
  b <- data.frame(covariate_id = c(1L, 2L), count_value = c(30, NA),
                  avg_value = c(4, 9), stdev_value = c(2, 1),
                  median_value = c(4, 9))
  out <- dsOMOPClient:::.pool_analysis_contract(list(a = a, b = b), contract)
  expect_identical(names(out$result), names(contract$columns))
  expect_equal(out$result$covariate_id, 1L)
  expect_equal(out$result$count_value, 50)
  expect_equal(out$result$avg_value, 3.2)
  expected_sd <- sqrt(((20 - 1) * 1^2 + (30 - 1) * 2^2 +
    20 * (2 - 3.2)^2 + 30 * (4 - 3.2)^2) / 49)
  expect_equal(out$result$stdev_value, expected_sd)
  expect_true(is.na(out$result$median_value))
  expect_true(any(grepl("sufficient statistics", out$warnings)))
})

test_that("effect estimates auto-pool by explicit strata and inverse variance", {
  contract <- list(
    version = 1L, strategy = "effect_estimate",
    columns = list(
      subgroup = .pc_col("key"), model_type = .pc_col("key"),
      target_persons = .pc_col("sum"),
      comparator_persons = .pc_col("sum"),
      log_estimate = .pc_col("nonpoolable", reason = "Inverse variance."),
      se_log_estimate = .pc_col("nonpoolable", reason = "Inverse variance.")
    ),
    strata = c("subgroup", "model_type"), log_estimate = "log_estimate",
    standard_error = "se_log_estimate", transform = "exp"
  )
  site <- function(a, b, se_a = .2, se_b = .25,
                   target = c(100, 80), comparator = c(90, 70),
                   model_type = "cox") data.frame(
    subgroup = c("A", "B"), model_type = model_type,
    target_persons = target, comparator_persons = comparator,
    log_estimate = c(a, b), se_log_estimate = c(se_a, se_b),
    stringsAsFactors = FALSE
  )
  frames <- list(
    one = site(log(1.5), log(2)),
    two = site(log(1.5), NA_real_, .2, NA_real_,
               target = c(120, 85), comparator = c(110, 75))
  )
  strict <- dsOMOPClient:::.pool_analysis_contract(
    frames, contract, policy = "strict"
  )
  expect_equal(strict$result$subgroup, "A")
  expect_equal(strict$result$model_type, "cox")
  expect_equal(strict$result$target_persons, 220)
  expect_equal(strict$result$comparator_persons, 200)
  expect_true(is.na(strict$result$log_estimate))
  expect_true(is.na(strict$result$se_log_estimate))
  expect_equal(strict$result$n_databases, 2L)
  expect_equal(strict$result$estimate_fixed, 1.5, tolerance = 1e-12)
  expect_equal(strict$result$i2, 0)

  permissive <- dsOMOPClient:::.pool_analysis_contract(
    frames, contract, policy = "pooled_only_ok"
  )
  expect_setequal(permissive$result$subgroup, c("A", "B"))
  expect_equal(permissive$result$n_databases[permissive$result$subgroup == "B"],
               1L)

  incomplete_strata <- contract
  incomplete_strata$strata <- "subgroup"
  expect_error(
    dsOMOPClient:::.validate_analysis_pooling_contract(incomplete_strata),
    "every key column"
  )

  invalid_role <- contract
  invalid_role$columns$log_estimate <- .pc_col("sum")
  expect_error(
    dsOMOPClient:::.validate_analysis_pooling_contract(invalid_role),
    "estimate/SE"
  )

  mixed_models <- list(
    one = site(log(1.5), log(2), model_type = "cox"),
    two = site(log(1.6), log(2.1), model_type = "poisson")
  )
  strict_mixed <- dsOMOPClient:::.pool_analysis_contract(
    mixed_models, contract, policy = "strict"
  )
  expect_null(strict_mixed$result)
  permissive_mixed <- dsOMOPClient:::.pool_analysis_contract(
    mixed_models, contract, policy = "pooled_only_ok"
  )
  expect_setequal(permissive_mixed$result$model_type, c("cox", "poisson"))
  expect_true(all(permissive_mixed$result$n_databases == 1L))
})

test_that("Kaplan-Meier pooling recomputes survival and truncates at a gap", {
  contract <- list(
    version = 1L, strategy = "kaplan_meier",
    columns = list(
      arm = .pc_col("key"), time_bin = .pc_col("key"),
      bin_start_days = .pc_col("key"), bin_end_days = .pc_col("key"),
      at_risk = .pc_col("sum"), events = .pc_col("sum"),
      survival_probability = .pc_col(
        "nonpoolable", reason = "Rebuilt from pooled risk sets."
      )
    ),
    strata = "arm", order = "time_bin", at_risk = "at_risk",
    events = "events", survival = "survival_probability",
    order_start = 1, order_step = 1
  )
  make <- function(time, risk, events) data.frame(
    arm = "target", time_bin = time, bin_start_days = (time - 1) * 30,
    bin_end_days = time * 30, at_risk = risk, events = events,
    survival_probability = cumprod(1 - events / risk)
  )
  frames <- list(
    a = make(1:3, c(100, 90, 80), c(10, 5, 4)),
    b = make(c(1, 3), c(50, 40), c(5, 2))
  )
  out <- dsOMOPClient:::.pool_analysis_contract(frames, contract)
  expect_equal(out$result$time_bin, 1)
  expect_equal(out$result$at_risk, 150)
  expect_equal(out$result$events, 15)
  expect_equal(out$result$survival_probability, .9)
  expect_true(any(grepl("truncated", out$warnings)))

  global_gap <- list(
    a = make(c(1, 3), c(100, 80), c(10, 4)),
    b = make(c(1, 3), c(50, 40), c(5, 2))
  )
  global_out <- dsOMOPClient:::.pool_analysis_contract(global_gap, contract)
  expect_equal(global_out$result$time_bin, 1)
  expect_true(any(grepl("contracted time bin", global_out$warnings)))

  invalid_grid <- contract
  invalid_grid$order_step <- 0
  expect_error(
    dsOMOPClient:::.validate_analysis_pooling_contract(invalid_grid),
    "positive"
  )
})

test_that("missing/invalid contracts fail closed while not_poolable explains why", {
  expect_error(
    dsOMOPClient:::.validate_analysis_pooling_contract(NULL),
    "Invalid analysis pooling contract"
  )
  malformed <- list(version = 1L, strategy = "tabular",
                    columns = list(n = .pc_col("mystery")))
  expect_error(
    dsOMOPClient:::.pool_analysis_contract(
      list(a = data.frame(n = 10), b = data.frame(n = 20)), malformed
    ),
    "unsupported role"
  )
  empty_km_strata <- list(
    version = 1L, strategy = "kaplan_meier",
    columns = list(
      time = .pc_col("key"), at_risk = .pc_col("sum"),
      events = .pc_col("sum"),
      survival = .pc_col("nonpoolable", reason = "Rebuilt from risk sets.")
    ),
    strata = character(), order = "time", at_risk = "at_risk",
    events = "events", survival = "survival",
    order_start = 1, order_step = 1
  )
  expect_error(
    dsOMOPClient:::.validate_analysis_pooling_contract(empty_km_strata),
    "strata"
  )
  closed <- list(version = 1L, strategy = "not_poolable",
                 reason = "Site-local quantile grids cannot be combined.")
  out <- dsOMOPClient:::.pool_analysis_contract(
    list(a = data.frame(x = 1), b = data.frame(x = 2)), closed
  )
  expect_null(out$result)
  expect_match(out$warnings, "quantile grids")
})

test_that("a valid one-server tabular result remains intact as the combined view", {
  contract <- list(
    version = 1L, strategy = "tabular",
    columns = list(
      group = .pc_col("key"), n = .pc_col("sum"),
      median = .pc_col("nonpoolable", reason = "Cross-site quantile.")
    )
  )
  frame <- data.frame(group = "all", n = 25, median = 7)
  out <- dsOMOPClient:::.pool_analysis_contract(list(site = frame), contract)
  expect_identical(out$result, frame)
  expect_length(out$warnings, 0L)
})

test_that("analysis.run exposes both/split/combine and preflights contracts", {
  conns <- list(a = "A", b = "B")
  assign("omop", list(conns = conns, res_symbol = "dsO.fake"),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)

  frames <- list(a = data.frame(group = "all", n = 10),
                 b = data.frame(group = "all", n = 20))
  metadata <- list(
    name = "dsomop:test", mode = "aggregate",
    pooling_contract = list(
      version = 1L, strategy = "tabular",
      columns = list(group = .pc_col("key"), n = .pc_col("sum"))
    )
  )
  runs <- 0L
  local_mocked_bindings(
    .session_harmonization_for_connections = function(...) list(
      max_analysis_scope_tables = 8L, poolable_counts = TRUE,
      count_band_width = 1L
    ),
    .package = "dsOMOPClient"
  )
  local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      head <- if (is.call(expr)) as.character(expr[[1L]]) else ""
      if (identical(head, "omopAnalysisGetDS")) {
        return(stats::setNames(list(metadata), server))
      }
      runs <<- runs + 1L
      stats::setNames(list(frames[[server]]), server)
    },
    .package = "DSI"
  )

  both <- ds.omop.analysis.run("dsomop:test")
  expect_named(both$per_site, c("a", "b"))
  expect_equal(both$pooled$n, 30)
  expect_equal(both$meta$type, "both")

  split <- ds.omop.analysis.run("dsomop:test", type = "s")
  expect_named(split$per_site, c("a", "b"))
  expect_null(split$pooled)
  expect_equal(split$meta$type, "split")

  combined <- ds.omop.analysis.run("dsomop:test", type = "combined")
  expect_length(combined$per_site, 0L)
  expect_equal(combined$pooled$n, 30)
  expect_equal(combined$meta$servers, c("a", "b"))

  metadata$pooling_contract <- list(
    version = 1L, strategy = "not_poolable",
    reason = "This estimand is site-specific."
  )
  site_specific <- ds.omop.analysis.run("dsomop:test", type = "both")
  expect_named(site_specific$per_site, c("a", "b"))
  expect_null(site_specific$pooled)
  expect_match(site_specific$meta$pooling_reason, "site-specific")

  metadata$pooling_contract <- NULL
  before <- runs
  expect_error(ds.omop.analysis.run("dsomop:test"),
               "Invalid analysis pooling contract")
  expect_equal(runs, before)
  expect_silent(split_without_contract <-
    ds.omop.analysis.run("dsomop:test", type = "split"))
  expect_named(split_without_contract$per_site, c("a", "b"))
})
