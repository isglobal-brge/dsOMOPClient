# ==============================================================================
# Test: Studio Concept Summary panel (UI structure + render helpers)
# ==============================================================================

test_that("concept summary UI renders the expected controls", {
  html <- as.character(.mod_explore_concept_summary_ui("cs"))
  expect_true(grepl("Concept Summary", html))
  expect_true(grepl("cs-table", html))        # table selectInput
  expect_true(grepl("cs-value_column", html)) # value column selectInput
  expect_true(grepl("cs-run_btn", html))      # Run button
  expect_true(grepl("Numeric value statistics", html))
  expect_true(grepl("Categorical value counts", html))
})

# A synthetic ds.omop.concept.summary() result matching the real shape:
#   numeric[[col]] = list(stats=<column.stats>, quantiles=<value.quantiles>)
#   categorical[[col]] = <value.counts>   (each inner result has $per_site/$pooled)
.cs_fixture <- function() {
  list(
    table = "measurement", concept_id = 3025315,
    numeric = list(value_as_number = list(
      stats = list(
        per_site = list(s1 = list(n_total = 30, mean = 72.6)),
        pooled   = list(n_total = 60, mean = 72.6)),
      quantiles = list(
        per_site = list(s1 = data.frame(probability = c(0.25, 0.5, 0.75),
                                        value = c(62, 71, 81))),
        pooled = NULL)
    )),
    categorical = list(value_as_concept_id = list(
      per_site = list(s1 = data.frame(value = c("High", "Low"), n = c(20, 10),
                                      stringsAsFactors = FALSE)),
      pooled = data.frame(value = c("High", "Low"), n = c(40, 20),
                          stringsAsFactors = FALSE))
    ))
}

test_that(".cs_pick_scope prefers pooled, falls back to first per_site", {
  st <- .cs_fixture()$numeric$value_as_number$stats
  expect_equal(.cs_pick_scope(st, "pooled")$n_total, 60)
  expect_equal(.cs_pick_scope(st, "per_site")$n_total, 30)
  q <- .cs_fixture()$numeric$value_as_number$quantiles
  # quantiles cannot be pooled (pooled is NULL) -> falls back to per_site
  expect_true(is.data.frame(.cs_pick_scope(q, "pooled")))
  expect_null(.cs_pick_scope(NULL, "pooled"))
})

test_that(".cs_numeric_table extracts n/mean/quantiles (no min/max)", {
  df <- .cs_numeric_table(.cs_fixture(), "pooled")
  expect_true(is.data.frame(df) && nrow(df) == 1)
  expect_equal(df$column, "value_as_number")
  expect_equal(df$n, 60)            # pooled count (summed across sites)
  expect_equal(df$mean, 72.6)
  expect_equal(df$median, 71)       # from quantiles (per_site fallback)
  expect_equal(df$p25, 62)
  expect_equal(df$p75, 81)
  expect_false(any(c("min", "max") %in% tolower(names(df))))  # DataSHIELD-safe
  expect_null(.cs_numeric_table(list(numeric = list()), "pooled"))
})

test_that(".cs_categorical_table returns the pooled value counts, or NULL when empty", {
  cc <- .cs_categorical_table(.cs_fixture(), "pooled")
  expect_true(is.data.frame(cc) && nrow(cc) == 2)
  expect_equal(cc$n, c(40, 20))     # pooled counts
  # a numeric-only concept (no categorical values) renders nothing
  expect_null(.cs_categorical_table(list(categorical = list()), "pooled"))
  empty <- list(categorical = list(value_as_concept_id =
                  list(per_site = list(s1 = data.frame()), pooled = data.frame())))
  expect_null(.cs_categorical_table(empty, "pooled"))
})
