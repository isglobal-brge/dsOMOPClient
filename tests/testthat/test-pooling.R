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
  expect_equal(nrow(result$result), 3)

  # Check top 3: B=130, A=100, D=90
  expect_equal(result$result$concept_id[1], 2)  # B = 50+80 = 130
  expect_equal(result$result$n_persons[1], 130)
  expect_equal(result$result$concept_id[2], 1)  # A = 100
  expect_equal(result$result$n_persons[2], 100)
  expect_equal(result$result$concept_id[3], 4)  # D = 90
  expect_equal(result$result$n_persons[3], 90)
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
  expect_equal(result$result$concept_name[result$result$concept_id == 1], "Alpha")
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

test_that(".pool_result returns NULL for unknown type", {
  result <- .pool_result(list(a = 1), "unknown_type", "strict")
  expect_null(result$result)
  expect_true(any(grepl("Unknown", result$warnings)))
})
