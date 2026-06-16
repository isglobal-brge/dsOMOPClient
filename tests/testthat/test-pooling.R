# ==============================================================================
# Tests for pooling formulas
# ==============================================================================

# --- .pool_counts -------------------------------------------------------------

test_that(".pool_counts sums correctly", {
  result <- .pool_counts(c(a = 100, b = 200, c = 300), "strict")
  expect_equal(result$result, 600)
  expect_equal(length(result$warnings), 0)
})

test_that(".pool_counts strict policy returns NULL when any NA", {
  result <- .pool_counts(c(a = 100, b = NA, c = 300), "strict")
  expect_null(result$result)
  expect_true(length(result$warnings) > 0)
  expect_true(grepl("Strict", result$warnings[1]))
})

test_that(".pool_counts pooled_only_ok policy skips NA servers", {
  result <- .pool_counts(c(a = 100, b = NA, c = 300), "pooled_only_ok")
  expect_equal(result$result, 400)
  expect_true(length(result$warnings) > 0)
  expect_true(grepl("Dropped", result$warnings[1]))
})

test_that(".pool_counts handles all NA", {
  result <- .pool_counts(c(a = NA_real_, b = NA_real_), "pooled_only_ok")
  expect_null(result$result)
})

test_that(".pool_counts handles single server", {
  result <- .pool_counts(c(a = 42), "strict")
  expect_equal(result$result, 42)
})

# --- .pool_means --------------------------------------------------------------

test_that(".pool_means weighted mean formula", {
  # Server A: n=100, mean=10; Server B: n=200, mean=20
  # Expected: (100*10 + 200*20) / 300 = 5000/300 = 16.667
  result <- .pool_means(c(a = 10, b = 20), c(a = 100, b = 200), "strict")
  expect_equal(result$result, 5000 / 300, tolerance = 1e-10)
  expect_equal(length(result$warnings), 0)
})

test_that(".pool_means strict with NA returns NULL", {
  result <- .pool_means(c(a = 10, b = NA), c(a = 100, b = 200), "strict")
  expect_null(result$result)
})

test_that(".pool_means pooled_only_ok skips NA", {
  result <- .pool_means(c(a = 10, b = NA), c(a = 100, b = 200), "pooled_only_ok")
  expect_equal(result$result, 10)
})

# --- .pool_variance -----------------------------------------------------------

test_that(".pool_variance Cochrane formula", {
  # Server A: n=100, mean=10, var=4; Server B: n=200, mean=20, var=9
  n <- c(a = 100, b = 200)
  m <- c(a = 10, b = 20)
  v <- c(a = 4, b = 9)
  result <- .pool_variance(v, m, n, "strict")

  # Manual calculation
  N <- sum(n)
  pooled_mean <- sum(n * m) / N
  within_ss <- sum((n - 1) * v)
  between_ss <- sum(n * (m - pooled_mean)^2)
  expected <- (within_ss + between_ss) / (N - 1)

  expect_equal(result$result, expected, tolerance = 1e-10)
})

test_that(".pool_variance strict with NA returns NULL", {
  result <- .pool_variance(c(a = 4, b = NA), c(a = 10, b = 20),
                           c(a = 100, b = 200), "strict")
  expect_null(result$result)
})

test_that(".pool_variance with single server returns server variance", {
  result <- .pool_variance(c(a = 4), c(a = 10), c(a = 100), "strict")
  # With single server: within_ss = 99*4, between_ss = 0
  expect_equal(result$result, 4, tolerance = 1e-10)
})

# --- .pool_effect_estimate (inverse-variance meta-analysis) -------------------

test_that(".pool_effect_estimate matches hand inverse-variance pool", {
  # Two sites: HR 1.5 (SE 0.2) and HR 2.0 (SE 0.3) on the log scale.
  le <- c(a = log(1.5), b = log(2.0))
  se <- c(a = 0.2, b = 0.3)
  out <- .pool_effect_estimate(le, se, "strict")
  w <- 1 / se^2
  fe <- sum(w * le) / sum(w)
  fese <- sqrt(1 / sum(w))
  expect_equal(out$result$n_databases, 2L)
  expect_equal(out$result$estimate_fixed, exp(fe), tolerance = 1e-10)
  expect_equal(out$result$ci_lo_fixed, exp(fe - qnorm(0.975) * fese),
               tolerance = 1e-10)
  expect_equal(out$result$ci_hi_fixed, exp(fe + qnorm(0.975) * fese),
               tolerance = 1e-10)
  # Q and I^2 are finite and non-negative; tau2 >= 0.
  expect_true(out$result$q >= 0 && out$result$i2 >= 0 && out$result$tau2 >= 0)
})

test_that(".pool_effect_estimate RE == FE when no heterogeneity (tau2=0)", {
  # Identical estimates -> Q = 0 -> I^2 = 0 -> tau2 = 0 -> RE collapses to FE.
  le <- c(a = log(1.7), b = log(1.7))
  se <- c(a = 0.25, b = 0.25)
  out <- .pool_effect_estimate(le, se, "strict")
  expect_equal(out$result$tau2, 0)
  expect_equal(out$result$i2, 0)
  expect_equal(out$result$estimate_random, out$result$estimate_fixed,
               tolerance = 1e-12)
})

test_that(".pool_effect_estimate strict fails closed on a suppressed site", {
  le <- c(a = log(1.5), b = NA_real_)
  se <- c(a = 0.2, b = NA_real_)
  out <- .pool_effect_estimate(le, se, "strict")
  expect_null(out$result)
  expect_true(any(grepl("Strict pooling failed", out$warnings)))
})

test_that(".pool_effect_estimate pooled_only_ok drops the suppressed site", {
  le <- c(a = log(1.5), b = NA_real_)
  se <- c(a = 0.2, b = NA_real_)
  out <- .pool_effect_estimate(le, se, "pooled_only_ok")
  expect_equal(out$result$n_databases, 1L)
  expect_equal(out$result$estimate_fixed, 1.5, tolerance = 1e-10)
  expect_true(any(grepl("Dropped server", out$warnings)))
})

test_that(".pool_effect_estimate rejects degenerate SE (<= 0)", {
  out <- .pool_effect_estimate(c(a = log(1.5), b = log(2)),
                               c(a = 0.2, b = 0), "strict")
  expect_null(out$result)
})

test_that(".pool_effect_estimate returns NULL when no valid sites", {
  out <- .pool_effect_estimate(c(a = NA_real_, b = NA_real_),
                               c(a = NA_real_, b = NA_real_), "pooled_only_ok")
  expect_null(out$result)
})

# --- .pool_proportions --------------------------------------------------------

test_that(".pool_proportions weighted proportions", {
  # Server A: 20/100, Server B: 60/200
  result <- .pool_proportions(c(a = 20, b = 60), c(a = 100, b = 200), "strict")
  expect_equal(result$result, 80 / 300, tolerance = 1e-10)
})

test_that(".pool_proportions strict with NA returns NULL", {
  result <- .pool_proportions(c(a = 20, b = NA), c(a = 100, b = 200), "strict")
  expect_null(result$result)
})

# --- .pool_histograms ---------------------------------------------------------

test_that(".pool_histograms bin-wise sum with aligned bins", {
  h1 <- data.frame(bin_start = c(0, 10, 20), bin_end = c(10, 20, 30),
                    count = c(5, 10, 3), suppressed = c(FALSE, FALSE, FALSE))
  h2 <- data.frame(bin_start = c(0, 10, 20), bin_end = c(10, 20, 30),
                    count = c(8, 12, 7), suppressed = c(FALSE, FALSE, FALSE))
  result <- .pool_histograms(list(a = h1, b = h2), "strict")
  expect_true(is.data.frame(result$result))
  expect_equal(result$result$count, c(13, 22, 10))
  expect_equal(result$result$suppressed, c(FALSE, FALSE, FALSE))
})

test_that(".pool_histograms propagates suppressed flags", {
  h1 <- data.frame(bin_start = c(0, 10), bin_end = c(10, 20),
                    count = c(5, NA), suppressed = c(FALSE, TRUE))
  h2 <- data.frame(bin_start = c(0, 10), bin_end = c(10, 20),
                    count = c(8, 12), suppressed = c(FALSE, FALSE))
  result <- .pool_histograms(list(a = h1, b = h2), "strict")
  expect_true(is.data.frame(result$result))
  expect_equal(result$result$count, c(13, 12))
  expect_equal(result$result$suppressed, c(FALSE, TRUE))
})

test_that(".pool_histograms strict fails on mismatched bins", {
  h1 <- data.frame(bin_start = c(0, 10), bin_end = c(10, 20),
                    count = c(5, 10), suppressed = c(FALSE, FALSE))
  h2 <- data.frame(bin_start = c(0, 10, 20), bin_end = c(10, 20, 30),
                    count = c(8, 12, 7), suppressed = c(FALSE, FALSE, FALSE))
  result <- .pool_histograms(list(a = h1, b = h2), "strict")
  expect_null(result$result)
  expect_true(any(grepl("mismatched", result$warnings)))
})

# --- .pool_top_k --------------------------------------------------------------

test_that(".pool_top_k two-pass merge", {
  df1 <- data.frame(concept_id = c(1, 2, 3), concept_name = c("A", "B", "C"),
                     n_persons = c(100, 50, 30))
  df2 <- data.frame(concept_id = c(2, 3, 4), concept_name = c("B", "C", "D"),
                     n_persons = c(80, 20, 90))

  result <- .pool_top_k(list(a = df1, b = df2), "n_persons", 3, "strict")
  expect_true(is.data.frame(result$result))
  # Strict: only concepts present on BOTH servers survive (summed). Concepts on
  # a single server are DROPPED entirely -- never surfaced, and no `suppressed`
  # column is built (a concept's mere appearance must not reveal it is rare).
  expect_equal(nrow(result$result), 2)
  expect_false("suppressed" %in% names(result$result))
  expect_equal(result$result$concept_id[1], 2)  # B = 50+80 = 130
  expect_equal(result$result$n_persons[1], 130)
  expect_equal(result$result$concept_id[2], 3)  # C = 30+20 = 50
  expect_equal(result$result$n_persons[2], 50)
  expect_false(any(c(1, 4) %in% result$result$concept_id))  # A, D dropped
})

test_that(".pool_top_k strict fails on invalid data", {
  result <- .pool_top_k(list(a = data.frame(x = 1)), "n_persons", 10, "strict")
  expect_null(result$result)
  expect_true(any(grepl("Strict", result$warnings)))
})

test_that(".pool_top_k carries concept_name", {
  df1 <- data.frame(concept_id = c(1, 2), concept_name = c("Alpha", "Beta"),
                     n = c(10, 20))
  df2 <- data.frame(concept_id = c(2, 3), concept_name = c("Beta", "Gamma"),
                     n = c(30, 5))
  result <- .pool_top_k(list(a = df1, b = df2), "n", 10, "strict")
  expect_true("concept_name" %in% names(result$result))
  # Only concept 2 (Beta) is on both servers -> survives with its name; the
  # single-server concepts (Alpha, Gamma) are dropped.
  expect_equal(result$result$concept_name[result$result$concept_id == 2], "Beta")
  expect_false(any(c(1, 3) %in% result$result$concept_id))
})

# --- .pool_result dispatcher --------------------------------------------------

test_that(".pool_result dispatches table_stats correctly", {
  per_site <- list(
    a = list(rows = 100, persons = 50, rows_suppressed = FALSE, persons_suppressed = FALSE),
    b = list(rows = 200, persons = 80, rows_suppressed = FALSE, persons_suppressed = FALSE)
  )
  result <- .pool_result(per_site, "table_stats", "strict")
  expect_equal(result$result$rows, 300)
  expect_equal(result$result$persons, 130)
})

test_that(".pool_result effect_estimate pools per-site CohortMethod arm frames", {
  # Each site returns the gated cm.effect_estimate frame: the log_estimate + SE
  # replicated across the target/comparator arm rows.
  mk <- function(le, se) data.frame(
    arm = c("target", "comparator"),
    log_estimate = le, se_log_estimate = se, stringsAsFactors = FALSE)
  per_site <- list(a = mk(log(1.5), 0.2), b = mk(log(2.0), 0.3))
  out <- .pool_result(per_site, "effect_estimate", "strict")
  w <- 1 / c(0.2, 0.3)^2
  fe <- sum(w * c(log(1.5), log(2.0))) / sum(w)
  expect_equal(out$result$n_databases, 2L)
  expect_equal(out$result$estimate_fixed, exp(fe), tolerance = 1e-10)
})

test_that(".pool_result effect_estimate reads SCCS log_irr/se_log_irr columns", {
  per_site <- list(
    a = data.frame(log_irr = log(3.0), se_log_irr = 0.25),
    b = data.frame(log_irr = log(2.0), se_log_irr = 0.25))
  out <- .pool_result(per_site, "effect_estimate", "strict")
  expect_equal(out$result$n_databases, 2L)
  # Equal weights -> geometric-mean-ish pool on the log scale.
  expect_equal(out$result$log_estimate_fixed,
               (log(3.0) + log(2.0)) / 2, tolerance = 1e-10)
})

test_that(".pool_result effect_estimate treats an empty/NA site as suppressed", {
  per_site <- list(
    a = data.frame(log_estimate = log(1.5), se_log_estimate = 0.2),
    b = data.frame(),  # server fail-closed -> empty frame
    c = data.frame(log_estimate = NA_real_, se_log_estimate = NA_real_))
  # strict: any suppressed site aborts.
  expect_null(.pool_result(per_site, "effect_estimate", "strict")$result)
  # pooled_only_ok: pool the one valid site.
  out <- .pool_result(per_site, "effect_estimate", "pooled_only_ok")
  expect_equal(out$result$n_databases, 1L)
  expect_equal(out$result$estimate_fixed, 1.5, tolerance = 1e-10)
})

test_that(".pool_concept_metadata dedupes shared vocab to one clean view", {
  one <- data.frame(concept_id = c(80180, 40481087),
                    concept_name = c("Osteoarthritis", "Viral sinusitis"),
                    stringsAsFactors = FALSE)
  out <- .pool_concept_metadata(list(nairobi = one, douala = one, dakar = one))
  expect_equal(nrow(out), 2L)                       # 3 identical copies -> 2 rows
  expect_setequal(out$concept_id, c(80180, 40481087))
  expect_null(.pool_concept_metadata(list(a = data.frame(), b = NULL)))
})

test_that(".pool_result ohdsi_results pools a MIXED-UNIT Table 1 (no stratum drop)", {
  # Regression: distribution-stat / proportion VALUE columns are NA on person-unit
  # rows of a Table 1. They must NOT become grouping keys, else split() drops every
  # stratum and the pooled view collapses to NULL. Counts must still sum.
  mk <- function(f, m) data.frame(
    characteristic = c("gender", "gender", "age"),
    level = c("FEMALE", "MALE", "mean"),
    unit = c("person", "person", "dist"),
    sum_value = c(f, m, NA),
    average = c(f / (f + m), m / (f + m), NA),
    count_value = c(NA, NA, f + m),
    avg_value = c(NA, NA, 65),        # dist stat: NA on person rows
    median_value = c(NA, NA, 64),
    stringsAsFactors = FALSE)
  per_site <- list(a = mk(10, 20), b = mk(15, 25))
  out <- .pool_result(per_site, "ohdsi_results", "strict")
  expect_false(is.null(out$result))
  g <- out$result[out$result$characteristic == "gender", ]
  expect_equal(nrow(g), 2L)                              # strata preserved
  expect_equal(g$sum_value[g$level == "FEMALE"], 25)     # 10 + 15 summed
  expect_equal(g$sum_value[g$level == "MALE"], 45)       # 20 + 25 summed
  expect_true(all(is.na(out$result$average)))            # proportions not summed
})

test_that(".pool_result column_stats pools n, mean, and a correct cross-site SD", {
  # Per-site stats as returned by .profileColumnStats (sd over non-NULL values).
  # Server A: n_total=110, n_missing=10 -> n_eff=100, mean=10, sd=2 (var=4)
  # Server B: n_total=210, n_missing=10 -> n_eff=200, mean=20, sd=3 (var=9)
  per_site <- list(
    a = list(n_total = 110, n_missing = 10, mean = 10, sd = 2),
    b = list(n_total = 210, n_missing = 10, mean = 20, sd = 3)
  )
  result <- .pool_result(per_site, "column_stats", "strict")

  expect_equal(result$result$n_total, 320)               # sum of n_total
  expect_equal(result$result$mean, (110 * 10 + 210 * 20) / 320, tolerance = 1e-10)

  # Pooled SD must equal sqrt of the sum-of-squares pooled variance over n_eff.
  n <- c(a = 100, b = 200); m <- c(a = 10, b = 20); v <- c(a = 4, b = 9)
  N <- sum(n); pooled_mean <- sum(n * m) / N
  expected_var <- (sum((n - 1) * v) + sum(n * (m - pooled_mean)^2)) / (N - 1)
  expect_equal(result$result$sd, sqrt(expected_var), tolerance = 1e-10)
})

test_that(".pool_result column_stats omits SD when no site reports one", {
  per_site <- list(
    a = list(n_total = 100, mean = 10),
    b = list(n_total = 200, mean = 20)
  )
  result <- .pool_result(per_site, "column_stats", "strict")
  expect_equal(result$result$n_total, 300)
  expect_null(result$result$sd)
})

test_that(".pool_result column_stats pools SD over the sites that report it", {
  # Server B suppressed its sd (small-sample NA); pooled sd should fall back to
  # the single reporting site (A) rather than vanish.
  per_site <- list(
    a = list(n_total = 100, n_missing = 0, mean = 10, sd = 2),
    b = list(n_total = 200, n_missing = 0, mean = 20, sd = NA_real_)
  )
  result <- .pool_result(per_site, "column_stats", "strict")
  expect_equal(result$result$sd, 2, tolerance = 1e-10)  # single-site var=4 -> sd=2
})

test_that(".pool_result column_stats never pools n_distinct across sites", {
  # n_distinct sets overlap across servers, so they cannot be summed; the pooled
  # view must expose n_total, mean, and sd but NOT n_distinct, even when every
  # site reports one.
  per_site <- list(
    a = list(n_total = 100, n_missing = 0, mean = 10, sd = 2, n_distinct = 40),
    b = list(n_total = 200, n_missing = 0, mean = 20, sd = 3, n_distinct = 55)
  )
  result <- .pool_result(per_site, "column_stats", "strict")
  expect_null(result$result$n_distinct)
  expect_equal(result$result$n_total, 300)
  expect_false(is.null(result$result$sd))           # sd still pooled
})

test_that(".pool_result column_stats SD weights use non-NULL counts (n_missing)", {
  # The server computes sd over NON-NULL values, so the pooled-variance weight is
  # n_total - n_missing. A two-site reference computed on n_eff must match.
  per_site <- list(
    a = list(n_total = 130, n_missing = 30, mean = 5,  sd = 1.5),   # n_eff = 100
    b = list(n_total = 250, n_missing = 50, mean = 15, sd = 4.0)    # n_eff = 200
  )
  result <- .pool_result(per_site, "column_stats", "strict")

  n <- c(a = 100, b = 200); m <- c(a = 5, b = 15); v <- c(a = 1.5^2, b = 4.0^2)
  N <- sum(n); pooled_mean <- sum(n * m) / N
  expected_var <- (sum((n - 1) * v) + sum(n * (m - pooled_mean)^2)) / (N - 1)
  expect_equal(result$result$sd, sqrt(expected_var), tolerance = 1e-10)
  expect_equal(result$result$n_total, 380)          # n_total summed, not n_eff
})

test_that(".pool_result returns NULL for unknown type", {
  result <- .pool_result(list(a = 1), "unknown_type", "strict")
  expect_null(result$result)
  expect_true(any(grepl("Unknown", result$warnings)))
})
