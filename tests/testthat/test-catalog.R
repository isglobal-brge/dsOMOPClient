# ==============================================================================
# Unit Tests: dsOMOPClient Catalog (Pooling Logic)
# ==============================================================================

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

# --- Pooling: ds.omop.catalog.pool --------------------------------------------

test_that("ds.omop.catalog.pool: returns NULL for empty results", {
  expect_null(ds.omop.catalog.pool(NULL))
  expect_null(ds.omop.catalog.pool(list()))
})

test_that("ds.omop.catalog.pool: single server returns as-is", {
  site_a <- data.frame(
    concept_id = c(1, 2, 3),
    n_persons = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  result <- ds.omop.catalog.pool(
    list(server_a = site_a),
    sensitive_fields = "n_persons"
  )

  expect_equal(result, site_a)
})

test_that("ds.omop.catalog.pool: sums counts across sites (strict)", {
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

  result <- ds.omop.catalog.pool(
    list(server_a = site_a, server_b = site_b),
    sensitive_fields = "n_persons",
    pool_strategy = "sum",
    policy = "strict"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$n_persons[result$concept_id == 1], 15)
  expect_equal(result$n_persons[result$concept_id == 2], 35)
  expect_equal(result$n_persons[result$concept_id == 3], 55)
})

test_that("ds.omop.catalog.pool: strict policy preserves NA", {
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

  result <- ds.omop.catalog.pool(
    list(server_a = site_a, server_b = site_b),
    sensitive_fields = "n_persons",
    pool_strategy = "sum",
    policy = "strict"
  )

  expect_equal(result$n_persons[result$concept_id == 1], 15)
  expect_true(is.na(result$n_persons[result$concept_id == 2]),
    info = "Suppressed cell should remain NA in strict mode")
})

test_that("ds.omop.catalog.pool: pooled_only_ok treats NA as 0", {
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

  result <- ds.omop.catalog.pool(
    list(server_a = site_a, server_b = site_b),
    sensitive_fields = "n_persons",
    pool_strategy = "sum",
    policy = "pooled_only_ok"
  )

  expect_equal(result$n_persons[result$concept_id == 1], 15)
  expect_equal(result$n_persons[result$concept_id == 2], 20)
})

test_that("ds.omop.catalog.pool: handles multiple sensitive fields", {
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

  result <- ds.omop.catalog.pool(
    list(server_a = site_a, server_b = site_b),
    sensitive_fields = c("n_persons", "n_records"),
    pool_strategy = "sum",
    policy = "strict"
  )

  expect_equal(result$n_persons[result$concept_id == 1], 18)
  expect_equal(result$n_records[result$concept_id == 1], 80)
})

test_that("ds.omop.catalog.pool: handles 3 servers", {
  sites <- list(
    a = data.frame(concept_id = 1:2, n_persons = c(10, 20),
                   stringsAsFactors = FALSE),
    b = data.frame(concept_id = 1:2, n_persons = c(5, 15),
                   stringsAsFactors = FALSE),
    c = data.frame(concept_id = 1:2, n_persons = c(3, 7),
                   stringsAsFactors = FALSE)
  )

  result <- ds.omop.catalog.pool(
    sites,
    sensitive_fields = "n_persons",
    pool_strategy = "sum",
    policy = "strict"
  )

  expect_equal(result$n_persons[result$concept_id == 1], 18)
  expect_equal(result$n_persons[result$concept_id == 2], 42)
})

test_that("ds.omop.catalog.pool: filters non-data.frame results", {
  results <- list(
    server_a = data.frame(x = 1:3, n = c(10, 20, 30),
                          stringsAsFactors = FALSE),
    server_b = "error occurred",
    server_c = NULL
  )

  result <- ds.omop.catalog.pool(
    results,
    sensitive_fields = "n"
  )

  # Should return server_a's data (only valid result)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that("ds.omop.catalog.pool: pool_strategy 'none' returns first", {
  site_a <- data.frame(concept_id = 1, n_persons = 10,
                       stringsAsFactors = FALSE)
  site_b <- data.frame(concept_id = 1, n_persons = 20,
                       stringsAsFactors = FALSE)

  result <- ds.omop.catalog.pool(
    list(server_a = site_a, server_b = site_b),
    pool_strategy = "none"
  )

  expect_equal(result$n_persons, 10)
})

test_that("ds.omop.catalog.pool: handles sites with different concepts", {
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

  result <- ds.omop.catalog.pool(
    list(server_a = site_a, server_b = site_b),
    sensitive_fields = "n_persons",
    pool_strategy = "sum",
    policy = "pooled_only_ok"
  )

  expect_true(is.data.frame(result))
  # Should have all 4 concepts (full outer join)
  expect_true(nrow(result) >= 3)
})
