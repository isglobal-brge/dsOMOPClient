.pooled_has_null <- function(x) {
  is.null(x) || (is.list(x) && any(vapply(x, .pooled_has_null, logical(1))))
}

test_that("column cardinalities are bounded, never treated as additive counts", {
  # The union is {a,b,c,d}, although each site's cardinality is two.
  values <- list(a = c("a", "b"), b = c("b", "c"), c = c("a", "d"))
  sites <- lapply(values, function(x) list(n_total = 20L, n_missing = 0L,
                                          n_distinct = length(unique(x))))
  pooled <- .pool_result(sites, "column_stats")$result
  expect_identical(pooled$n_distinct, NA_real_)
  expect_equal(pooled$n_distinct_max, 2)
  expect_equal(pooled$n_distinct_sum, 6)
  expect_lte(pooled$n_distinct_max, length(unique(unlist(values))))
  expect_gte(pooled$n_distinct_sum, length(unique(unlist(values))))
  expect_false("mean" %in% names(pooled))
  expect_false("sd" %in% names(pooled))
  expect_true(all(names(sites[[1]]) %in% names(pooled)))
  expect_equal(nrow(as.data.frame(pooled)), 1L)
  for (policy in c("strict", "pooled_only_ok")) {
    unknown <- sites
    unknown$c$n_distinct <- NA_real_
    result <- .pool_result(unknown, "column_stats", policy)$result
    expect_true(is.na(result$n_distinct_max))
    expect_true(is.na(result$n_distinct_sum))
    unknown <- sites[-3L]
    attr(unknown, "ds_errors") <- list(c = "unavailable")
    result <- .pool_result(unknown, "column_stats", policy)$result
    if (policy == "strict") expect_null(result) else {
      expect_true(is.na(result$n_distinct_max))
      expect_true(is.na(result$n_distinct_sum))
    }
  }
})

test_that("numeric summaries preserve suppressed SD and unknown contributor counts", {
  sites <- list(a = list(n_total = 100, n_persons = 10, n_missing = 0,
                         n_distinct = 2, mean = 1, sd = 2),
                b = list(n_total = 20, n_persons = 20, n_missing = 0,
                         n_distinct = 3, mean = 4, sd = 3),
                c = list(n_total = 90, n_persons = 30, n_missing = 0,
                         n_distinct = 4, mean = 7, sd = 4))
  result <- .pool_result(sites, "column_stats")$result
  expect_equal(result$mean, 5) # person-weighted, not record-weighted
  expected_sd <- sqrt((9*4 + 19*9 + 29*16 + 10*16 + 20 + 30*4) / 59)
  expect_equal(result$sd, expected_sd)
  sites$c$sd <- NA_real_
  result <- .pool_result(sites, "column_stats")$result
  expect_identical(result$sd, NA_real_)
  expect_false(.pooled_has_null(result))
  sites$c$n_missing <- NULL
  result <- .pool_result(sites, "column_stats")$result
  expect_identical(result$mean, NA_real_)
  expect_identical(result$n_missing, NA_real_)
})

test_that("missing count columns stay unknown without losing released records", {
  cases <- list(
    value_counts = list(data.frame(value = "A", n = 10, n_persons = 5), "n"),
    concept_prevalence = list(data.frame(concept_id = 1L, n_persons = 10,
                                         n_records = 20), "n_persons"),
    date_counts = list(data.frame(period = "2020", n_records = 10, n_persons = 5), "n_records"),
    domain_coverage = list(data.frame(table_name = "observation", n_records = 10,
                                      n_persons = 5), "n_records"),
    concept_locate = list(data.frame(concept_id = 1L, table_name = "observation",
                                     concept_column = "observation_concept_id",
                                     n_records = 10, n_persons = 5), "n_records")
  )
  for (kind in names(cases)) {
    metric <- cases[[kind]][[2]]
    other <- if (kind == "concept_prevalence") "n_records" else "n_persons"
    sites <- setNames(rep(list(cases[[kind]][[1]]), 3), c("a", "b", "c"))
    for (variant in c("absent", "NA")) {
      sites$c[[other]] <- if (variant == "absent") NULL else NA_real_
      for (policy in c("strict", "pooled_only_ok")) {
        result <- .pool_result(sites, kind, policy)$result
        expect_true(other %in% names(result), info = paste(kind, variant, policy))
        expect_true(all(is.na(result[[other]])))
        expect_equal(result[[metric]], 3 * cases[[kind]][[1]][[metric]])
      }
    }
  }
})

test_that("empty value counts cannot silently become a two-site strict pool", {
  frame <- data.frame(value = "A", n = 10, n_persons = 5)
  sites <- list(a = frame, b = frame, c = frame[FALSE, ])
  strict <- .pool_result(sites, "value_counts")
  expect_null(strict$result)
  expect_match(paste(strict$warnings, collapse = " "), "server c")
  partial <- .pool_result(sites, "value_counts", "pooled_only_ok")
  expect_equal(partial$result$n, 20)
  expect_match(paste(partial$warnings, collapse = " "), "Dropped server")
})

test_that("strict scalar helpers do not align away missing contributors", {
  expect_null(.pool_counts(list(a = 10, b = NULL, c = 20))$result)
  expect_null(.pool_means(c(a = 1, b = 2), c(a = 10))$result)
  expect_null(.pool_means(list(a = 1, b = NULL), list(a = 10, b = NULL))$result)
  expect_null(.pool_variance(c(a = 1), c(a = 1, b = 2), c(a = 10, b = 20))$result)
  expect_null(.pool_proportions(c(a = 2), c(a = 10, b = 20))$result)
  expect_null(.pool_effect_estimate(c(a = 1, b = 2), c(a = .2))$result)
})

test_that("drilldown components preserve the complete federation requirement", {
  profile <- list(summary = list(concept_id = 1L, concept_name = "A",
    n_records = 10, n_persons = 5, records_per_person_mean = 2,
    pct_persons_multi = 30), numeric_summary = list(
      histogram = data.frame(bin_start = 0, bin_end = 1, count = 10),
      quantiles = list(p25 = 0.2, median = 0.5, p75 = 0.8)),
    categorical_values = data.frame(value_as_concept_id = 2L, n = 10),
    missingness = data.frame(column_name = "x", n_total = 10,
                             n_missing = 5, missing_rate = .5), date_range = NULL)
  sites <- list(a = profile, b = profile, c = profile)
  sites$c$numeric_summary <- NULL
  sites$c$categorical_values <- NULL
  sites$c$missingness <- NULL
  result <- .pool_result(sites, "concept_drilldown")
  expect_false(.pooled_has_null(result$result))
  expect_true(all(is.na(result$result$numeric_summary$histogram$count)))
  expect_true(all(is.na(result$result$categorical_values$n)))
  expect_true(all(is.na(result$result$missingness$n_total)))
  expect_true(is.na(result$result$summary$pct_persons_multi))
  expect_equal(result$result$summary$n_records, 30)
  expect_match(paste(result$warnings, collapse = " "), "server c")
  expect_equal(nrow(as.data.frame(result$result)), 1L)
})

test_that("central shape preserves types and nested components without NULL leaves", {
  schema <- list(a = list(n = 10L, label = "a", day = as.Date("2020-01-01"),
                          detail = list(median = 4, optional = NULL)),
                 b = list(n = 20L, label = "b", day = as.Date("2020-01-02"),
                          detail = list(median = 6, optional = NULL)))
  result <- dsomop_result(schema, list(n = 30L, label = NULL, detail = NULL))
  expect_false(.pooled_has_null(result$pooled))
  expect_identical(result$pooled$label, NA_character_)
  expect_s3_class(result$pooled$day, "Date")
  expect_true(is.na(result$pooled$detail$median))
  expect_match(paste(result$meta$warnings, collapse = " "), "without a pooling rule")
  expect_equal(nrow(as.data.frame(result$pooled)), 1L)
  expect_equal(nrow(.stack_per_site(schema)), 2L)
  expect_null(dsomop_result(schema, NULL)$pooled) # unavailable whole view
  wire <- dsomop_result(list(a = list(protocol = "wire", payload = list(n = 10))),
                        list(n = 10, optional = NULL), schema = list())
  expect_false("protocol" %in% names(wire$pooled))
  expect_false(.pooled_has_null(wire$pooled))
  mixed <- .complete_pooled_shape(list(a = 1, NULL), list())$result
  expect_false(.pooled_has_null(mixed))
  expect_length(mixed, 2L)
  expect_identical(as.data.frame(mixed)$V2, NA)
})

test_that("every core pooling strategy preserves shape and converts all views", {
  summary <- list(n_total = 20, n_missing = 0, n_distinct = 2)
  fixtures <- list(
    table_stats = list(rows = 20, persons = 10),
    column_stats = summary,
    domain_coverage = data.frame(table_name = "observation", n_persons = 10),
    missingness = data.frame(column_name = "x", n_total = 20, n_missing = 5,
                             missing_rate = .25),
    value_counts = data.frame(value = "A", n = 20, n_persons = 10),
    concept_prevalence = data.frame(concept_id = 1L, n_records = 20, n_persons = 10),
    histogram = data.frame(bin_start = 0, bin_end = 1, count = 20),
    date_counts = data.frame(period = "2020", n_records = 20, n_persons = 10),
    concept_drilldown = list(summary = list(concept_id = 1L, concept_name = "A",
      n_records = 20, n_persons = 10, records_per_person_mean = 2,
      pct_persons_multi = 30), numeric_summary = NULL, categorical_values = NULL,
      date_range = NULL, missingness = NULL),
    concept_locate = data.frame(concept_id = 1L, table_name = "observation",
      concept_column = "observation_concept_id", n_records = 20, n_persons = 10),
    achilles_results = data.frame(analysis_id = 1L, stratum_1 = "A", count_value = 20),
    achilles_distribution = data.frame(analysis_id = 1L, stratum_1 = "A",
      count_value = 20, avg_value = 2, stdev_value = 1, median_value = 3,
      p10_value = 1, p25_value = 1, p75_value = 4, p90_value = 5),
    crosstab = list(row_col = "x", col_col = "y", count_mode = "persons",
      row_levels = c("A", "B"), col_levels = c("C", "D"),
      counts = matrix(c(10,20,30,40), 2, dimnames=list(c("A","B"),c("C","D"))))
  )
  for (kind in names(fixtures)) {
    sites <- setNames(rep(list(fixtures[[kind]]), 3), c("a", "b", "c"))
    pool <- .pool_result(sites, kind)
    expect_false(is.null(pool$result), info = kind)
    expect_false(.pooled_has_null(pool$result), info = kind)
    for (type in c("split", "combine", "both")) {
      result <- .result_type_view(dsomop_result(sites, pool$result), type)
      frame <- as.data.frame(result)
      expect_s3_class(frame, "data.frame")
      expect_gt(nrow(frame), 0L)
      expect_output(print(result), "dsomop_result")
      if (type == "split") expect_setequal(frame$server, names(sites)) else {
        expect_s3_class(as.data.frame(result$pooled), "data.frame")
      }
    }
    if (is.data.frame(fixtures[[kind]])) {
      counts <- intersect(c("n_total", "n_missing", "n", "n_records",
                             "n_persons", "count", "count_value"), names(fixtures[[kind]]))
      expect_true(all(counts %in% names(pool$result)), info = kind)
      for (column in counts) expect_equal(pool$result[[column]], 3 * fixtures[[kind]][[column]])
    }
    if (kind == "missingness") expect_equal(pool$result$missing_rate, .25)
    if (kind == "achilles_distribution") {
      expect_equal(pool$result$avg_value, 2)
      expect_true(all(is.na(unlist(pool$result[c("median_value", "p10_value", "p25_value",
                                                "p75_value", "p90_value")]))))
    }
  }
})
