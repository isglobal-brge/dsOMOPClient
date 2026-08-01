# ==============================================================================
# Unit Tests: dsOMOPClient Query Templates (Pooling Logic)
# ==============================================================================

.query_pool_deprecation <- "'ds\\.omop\\.query\\.pool' is deprecated"

# --- Pooling: .pool_col -------------------------------------------------------

test_that(".pool_col: strict policy returns NA when either is NA", {
  result <- dsOMOPClient:::.pool_col(c(5, NA, 10), c(3, 7, NA), "strict")

  expect_equal(result[1], 8)
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
})

test_that(".pool_col: pooled_only_ok policy treats NA as 0", {
  result <- dsOMOPClient:::.pool_col(c(5, NA, 10), c(3, 7, NA), "pooled_only_ok")

  expect_equal(result[1], 8)
  expect_equal(result[2], 7)
  expect_equal(result[3], 10)
})

test_that(".pool_col: sums correctly with no NAs", {
  result <- dsOMOPClient:::.pool_col(c(1, 2, 3), c(4, 5, 6), "strict")
  expect_equal(result, c(5, 7, 9))
})

# --- Pooling: ds.omop.query.pool -----------------------------------------------

test_that("ds.omop.query.pool: returns NULL for empty results", {
  expect_warning(
    expect_null(ds.omop.query.pool(NULL)),
    regexp = .query_pool_deprecation
  )
  expect_warning(
    expect_null(ds.omop.query.pool(list())),
    regexp = .query_pool_deprecation
  )
})

test_that("deprecated query pooling rejects policy and strategy typos", {
  results <- list(
    a = data.frame(group = "x", n_persons = 10),
    b = data.frame(group = "x", n_persons = 20)
  )
  expect_error(
    suppressWarnings(ds.omop.query.pool(results, policy = "strcit")),
    "should be one of"
  )
  expect_error(
    suppressWarnings(ds.omop.query.pool(results, pool_strategy = "average")),
    "should be one of"
  )
  expect_error(
    suppressWarnings(ds.omop.query.pool(
      results, pool_strategy = "weighted_mean"
    )),
    "cannot safely infer"
  )
  expect_error(.pool_col(1, NA, "strcit"), "should be one of")
})

test_that("ds.omop.query.pool: single server returns as-is", {
  site_a <- data.frame(
    concept_id = c(1, 2, 3),
    n_persons = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- ds.omop.query.pool(
      list(server_a = site_a),
      sensitive_fields = "n_persons"
    ),
    regexp = .query_pool_deprecation
  )

  expect_equal(result, site_a)
})

test_that("ds.omop.query.pool: sums counts across sites (strict)", {
  site_a <- data.frame(
    concept_id = c(1, 2, 3),
    concept_name = c("A", "B", "C"),
    n_persons = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  site_b <- data.frame(
    concept_id = c(1, 2, 3),
    concept_name = c("A", "B", "C"),
    n_persons = c(5, 15, 25),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- ds.omop.query.pool(
      list(server_a = site_a, server_b = site_b),
      sensitive_fields = "n_persons",
      pool_strategy = "sum",
      policy = "strict"
    ),
    regexp = .query_pool_deprecation
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$n_persons[result$concept_id == 1], 15)
  expect_equal(result$n_persons[result$concept_id == 2], 35)
  expect_equal(result$n_persons[result$concept_id == 3], 55)
})

test_that("ds.omop.query.pool: strict policy preserves NA", {
  site_a <- data.frame(
    concept_id = c(1, 2),
    n_persons = c(10, NA),  # NA = suppressed
    stringsAsFactors = FALSE
  )
  site_b <- data.frame(
    concept_id = c(1, 2),
    n_persons = c(5, 20),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- ds.omop.query.pool(
      list(server_a = site_a, server_b = site_b),
      sensitive_fields = "n_persons",
      pool_strategy = "sum",
      policy = "strict"
    ),
    regexp = .query_pool_deprecation
  )

  expect_equal(result$n_persons[result$concept_id == 1], 15)
  expect_true(is.na(result$n_persons[result$concept_id == 2]),
    info = "Suppressed cell should remain NA in strict mode")
})

test_that("ds.omop.query.pool: pooled_only_ok treats NA as 0", {
  site_a <- data.frame(
    concept_id = c(1, 2),
    n_persons = c(10, NA),
    stringsAsFactors = FALSE
  )
  site_b <- data.frame(
    concept_id = c(1, 2),
    n_persons = c(5, 20),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- ds.omop.query.pool(
      list(server_a = site_a, server_b = site_b),
      sensitive_fields = "n_persons",
      pool_strategy = "sum",
      policy = "pooled_only_ok"
    ),
    regexp = .query_pool_deprecation
  )

  expect_equal(result$n_persons[result$concept_id == 1], 15)
  expect_equal(result$n_persons[result$concept_id == 2], 20)
})

test_that("ds.omop.query.pool: handles multiple sensitive fields", {
  site_a <- data.frame(
    concept_id = c(1, 2),
    n_persons = c(10, 20),
    n_records = c(50, 100),
    stringsAsFactors = FALSE
  )
  site_b <- data.frame(
    concept_id = c(1, 2),
    n_persons = c(8, 12),
    n_records = c(30, 60),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- ds.omop.query.pool(
      list(server_a = site_a, server_b = site_b),
      sensitive_fields = c("n_persons", "n_records"),
      pool_strategy = "sum",
      policy = "strict"
    ),
    regexp = .query_pool_deprecation
  )

  expect_equal(result$n_persons[result$concept_id == 1], 18)
  expect_equal(result$n_records[result$concept_id == 1], 80)
})

test_that("ds.omop.query.pool: handles 3 servers", {
  sites <- list(
    a = data.frame(concept_id = 1:2, n_persons = c(10, 20),
                   stringsAsFactors = FALSE),
    b = data.frame(concept_id = 1:2, n_persons = c(5, 15),
                   stringsAsFactors = FALSE),
    c = data.frame(concept_id = 1:2, n_persons = c(3, 7),
                   stringsAsFactors = FALSE)
  )

  expect_warning(
    result <- ds.omop.query.pool(
      sites,
      sensitive_fields = "n_persons",
      pool_strategy = "sum",
      policy = "strict"
    ),
    regexp = .query_pool_deprecation
  )

  expect_equal(result$n_persons[result$concept_id == 1], 18)
  expect_equal(result$n_persons[result$concept_id == 2], 42)
})

test_that("ds.omop.query.pool: pooled_only_ok filters non-data.frame results", {
  results <- list(
    server_a = data.frame(x = 1:3, n = c(10, 20, 30),
                          stringsAsFactors = FALSE),
    server_b = "error occurred",
    server_c = NULL
  )

  expect_warning(
    result <- ds.omop.query.pool(
      results,
      sensitive_fields = "n",
      policy = "pooled_only_ok"
    ),
    regexp = .query_pool_deprecation
  )

  # Should return server_a's data (only valid result)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that("deprecated strict query pooling never publishes a partial federation", {
  results <- list(
    server_a = data.frame(group = "x", n_persons = 10),
    server_b = "server failed"
  )
  expect_null(suppressWarnings(ds.omop.query.pool(
    results, sensitive_fields = "n_persons", policy = "strict"
  )))

  complete <- list(server_a = results$server_a)
  attr(complete, "ds_errors") <- list(server_b = "unavailable")
  expect_null(suppressWarnings(ds.omop.query.pool(
    complete, sensitive_fields = "n_persons", policy = "strict"
  )))
})

test_that("deprecated query pool validates weighted_mean before one-site return", {
  one <- list(server_a = data.frame(group = "x", n_persons = 10))
  expect_error(
    suppressWarnings(ds.omop.query.pool(
      one, sensitive_fields = "n_persons", pool_strategy = "weighted_mean"
    )),
    "cannot safely infer"
  )
})

test_that("ds.omop.query.pool: pool_strategy 'none' returns first", {
  site_a <- data.frame(concept_id = 1, n_persons = 10,
                       stringsAsFactors = FALSE)
  site_b <- data.frame(concept_id = 1, n_persons = 20,
                       stringsAsFactors = FALSE)

  expect_warning(
    result <- ds.omop.query.pool(
      list(server_a = site_a, server_b = site_b),
      pool_strategy = "none"
    ),
    regexp = .query_pool_deprecation
  )

  expect_equal(result$n_persons, 10)
})

test_that("ds.omop.query.pool: handles sites with different concepts", {
  site_a <- data.frame(
    concept_id = c(1, 2, 3),
    concept_name = c("A", "B", "C"),
    n_persons = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  site_b <- data.frame(
    concept_id = c(2, 3, 4),
    concept_name = c("B", "C", "D"),
    n_persons = c(15, 25, 40),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- ds.omop.query.pool(
      list(server_a = site_a, server_b = site_b),
      sensitive_fields = "n_persons",
      pool_strategy = "sum",
      policy = "pooled_only_ok"
    ),
    regexp = .query_pool_deprecation
  )

  expect_true(is.data.frame(result))
  # Should have all 4 concepts (full outer join)
  expect_true(nrow(result) >= 3)
})
