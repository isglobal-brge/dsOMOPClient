# ==============================================================================
# Test: Studio Explore fixes — disclosure-safe pooled fallback, session cache,
# and the per-concept Numeric Distribution picker.
# ==============================================================================

.fix_result <- function(pooled) {
  structure(list(
    per_site = list(
      nairobi = data.frame(concept_id = 1L, n_persons = 30L),
      douala  = data.frame(concept_id = 2L, n_persons = 25L)),
    pooled = pooled),
    class = "dsomop_result")
}

test_that(".explore_display returns pooled untouched when it has data", {
  res <- .fix_result(data.frame(concept_id = 1L, n_persons = 55L))
  out <- .explore_display(res, "pooled", c("nairobi", "douala"),
                          count_cols = "n_persons")
  expect_equal(nrow(out), 1L)
  expect_equal(out$n_persons, 55L)
  expect_null(attr(out, "pooled_fallback"))
})

test_that(".explore_display falls back to per-site rows when pooled is empty", {
  res <- .fix_result(data.frame(concept_id = integer(0), n_persons = integer(0)))
  out <- .explore_display(res, "pooled", c("nairobi", "douala"),
                          count_cols = "n_persons")
  # Row-bound per-site view: both sites' rows, with a server column.
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 2L)
  expect_true("server" %in% names(out))
  expect_true(isTRUE(attr(out, "pooled_fallback")))
})

test_that(".explore_display falls back when pooled counts are all NA", {
  res <- .fix_result(data.frame(concept_id = c(1L, 2L),
                                n_persons = c(NA_integer_, NA_integer_)))
  out <- .explore_display(res, "pooled", c("nairobi", "douala"),
                          count_cols = "n_persons")
  expect_true(isTRUE(attr(out, "pooled_fallback")))
  expect_equal(nrow(out), 2L)
})

test_that(".explore_display does not fall back under per_site scope", {
  res <- .fix_result(NULL)
  out <- .explore_display(res, "per_site", "nairobi", count_cols = "n_persons")
  expect_equal(out$n_persons, 30L)
  expect_null(attr(out, "pooled_fallback"))
})

test_that(".pooled_fallback_banner only renders on the fallback flag", {
  plain <- data.frame(a = 1)
  expect_null(.pooled_fallback_banner(plain))
  flagged <- structure(data.frame(a = 1), pooled_fallback = TRUE)
  html <- as.character(.pooled_fallback_banner(flagged))
  expect_true(grepl("Strict pooling suppressed", html))
  expect_true(grepl("Best Effort", html))
})

test_that(".cache_key is order-independent and NULL-sensitive", {
  k1 <- .cache_key(list(fn = "f", table = "m", concept_id = 5L))
  k2 <- .cache_key(list(concept_id = 5L, table = "m", fn = "f"))
  expect_identical(k1, k2)
  k_null <- .cache_key(list(fn = "f", table = "m", concept_id = NULL))
  k_set  <- .cache_key(list(fn = "f", table = "m", concept_id = 5L))
  expect_false(identical(k_null, k_set))
})

test_that(".cached_call memoises and keys on the selected servers", {
  st <- list(query_cache = new.env(parent = emptyenv()),
             selected_servers = c("nairobi", "douala"), symbol = "omop")
  n <- 0L
  compute <- function() { n <<- n + 1L; list(v = n) }
  r1 <- .cached_call(st, list(fn = "f", table = "m"), compute)
  r2 <- .cached_call(st, list(fn = "f", table = "m"), compute)
  expect_identical(r1, r2)
  expect_equal(n, 1L)                         # second call is a cache hit
  # Different server selection -> different key -> recompute.
  st$selected_servers <- c("nairobi")
  r3 <- .cached_call(st, list(fn = "f", table = "m"), compute)
  expect_equal(n, 2L)
  expect_equal(r3$v, 2L)
})

test_that(".cached_call does not cache NULL results", {
  st <- list(query_cache = new.env(parent = emptyenv()),
             selected_servers = "nairobi", symbol = "omop")
  n <- 0L
  compute <- function() { n <<- n + 1L; NULL }
  .cached_call(st, list(fn = "f"), compute)
  .cached_call(st, list(fn = "f"), compute)
  expect_equal(n, 2L)                          # NULL never cached -> recompute
})

test_that("Numeric Distribution UI embeds a required concept picker", {
  html <- as.character(.mod_explore_numeric_ui("num"))
  # Concept picker is namespaced under explore-numeric-concept-*
  expect_true(grepl("num-concept-search_term", html))
  expect_true(grepl("Concept \\(required\\)", html))
  expect_true(grepl("Heart rate", html))       # guidance example
})

test_that("ds.omop.value.histogram exposes a concept_id argument", {
  expect_true("concept_id" %in% names(formals(ds.omop.value.histogram)))
})

test_that("ds.omop.concept.summary threads pooling_policy", {
  expect_true("pooling_policy" %in% names(formals(ds.omop.concept.summary)))
})
