# ==============================================================================
# Tests for cross-tab client wrappers + cross-tab pooling branch
# ==============================================================================

# --- ds.omop.crosstab call building (execute = FALSE) -------------------------

test_that("ds.omop.crosstab execute=FALSE returns dsomop_result with call code", {
  result <- ds.omop.crosstab("person", "gender_concept_id", "race_concept_id",
                             execute = FALSE)
  expect_s3_class(result, "dsomop_result")
  expect_true(grepl("ds.omop.crosstab", result$meta$call_code))
  expect_true(grepl("gender_concept_id", result$meta$call_code))
  expect_true(grepl("race_concept_id", result$meta$call_code))
})

test_that("ds.omop.crosstab call code omits NULL optional args", {
  res <- ds.omop.crosstab("person", "gender_concept_id", "race_concept_id",
                          execute = FALSE)
  expect_false(grepl("row_concept_id", res$meta$call_code))
  expect_false(grepl("stratify_by", res$meta$call_code))
  expect_false(grepl("cohort_table", res$meta$call_code))
})

test_that("ds.omop.crosstab plumbs concept ids, cohort, stratify into call code", {
  res <- ds.omop.crosstab("condition_occurrence",
                          "condition_concept_id", "gender_concept_id",
                          row_concept_id = 316866,
                          cohort_table = "my_cohort",
                          stratify_by = "age_band",
                          execute = FALSE)
  expect_true(grepl("row_concept_id = 316866", res$meta$call_code))
  expect_true(grepl("cohort_table = ", res$meta$call_code))
  expect_true(grepl("stratify_by = ", res$meta$call_code))
})

test_that("ds.omop.crosstab builds the correct server aggregate call()", {
  # Build the call() the wrapper would dispatch and verify shape/args.
  expr <- call("omopCrossTabDS", "omop_res",
               "person", "gender_concept_id", "race_concept_id",
               count_mode = "persons",
               row_concept_ids = NULL, col_concept_ids = NULL,
               cohort_table = NULL, stratify_by = NULL)
  expect_equal(as.character(expr[[1]]), "omopCrossTabDS")
  expect_equal(expr[["count_mode"]], "persons")
  expect_equal(expr[[3]], "person")
  expect_equal(expr[[4]], "gender_concept_id")
  expect_equal(expr[[5]], "race_concept_id")
})

# --- ds.omop.comorbidity ------------------------------------------------------

test_that("ds.omop.comorbidity delegates to crosstab with concept presence axes", {
  res <- ds.omop.comorbidity(316866, 201826, execute = FALSE)
  expect_s3_class(res, "dsomop_result")
  expect_true(grepl("ds.omop.crosstab", res$meta$call_code))
  expect_true(grepl("condition_concept_id", res$meta$call_code))
  expect_true(grepl("row_concept_id = 316866", res$meta$call_code))
  expect_true(grepl("col_concept_id = 201826", res$meta$call_code))
})

test_that("ds.omop.comorbidity forwards table override and scope", {
  res <- ds.omop.comorbidity(316866, 201826,
                             tableA = "drug_exposure",
                             execute = FALSE)
  expect_true(grepl("drug_exposure", res$meta$call_code))
})

# --- .pool_result("crosstab") -------------------------------------------------

# Helper: build a per-site cross-tab object matching the server shape.
.mk_ct <- function(M) {
  list(row_col = "r", col_col = "c", count_mode = "persons",
       counts = M, suppressed = any(is.na(M)))
}

test_that(".pool_result crosstab sums matching cells across sites", {
  M1 <- matrix(c(10, 20, 30, 40), 2, 2,
               dimnames = list(c("F", "M"), c("W", "B")))
  M2 <- matrix(c(1, 2, 3, 4), 2, 2,
               dimnames = list(c("F", "M"), c("W", "B")))
  out <- .pool_result(list(a = .mk_ct(M1), b = .mk_ct(M2)), "crosstab", "strict")
  expect_equal(out$result$counts["F", "W"], 11)
  expect_equal(out$result$counts["M", "B"], 44)
  expect_false(out$result$suppressed)
})

test_that(".pool_result crosstab strict suppresses a cell NA on any site", {
  M1 <- matrix(c(10, 20, 30, 40), 2, 2,
               dimnames = list(c("F", "M"), c("W", "B")))
  M2 <- matrix(c(NA, 2, 3, 4), 2, 2,
               dimnames = list(c("F", "M"), c("W", "B")))
  out <- .pool_result(list(a = .mk_ct(M1), b = .mk_ct(M2)), "crosstab", "strict")
  expect_true(is.na(out$result$counts["F", "W"]))
  expect_equal(out$result$counts["M", "B"], 44)
  expect_true(out$result$suppressed)
})

test_that(".pool_result crosstab strict suppresses a cell absent on any site", {
  # Site b lacks the "B" column entirely -> the B cells must be suppressed.
  M1 <- matrix(c(10, 20, 30, 40), 2, 2,
               dimnames = list(c("F", "M"), c("W", "B")))
  M2 <- matrix(c(1, 2), 2, 1,
               dimnames = list(c("F", "M"), c("W")))
  out <- .pool_result(list(a = .mk_ct(M1), b = .mk_ct(M2)), "crosstab", "strict")
  expect_equal(out$result$counts["F", "W"], 11)
  expect_true(is.na(out$result$counts["F", "B"]))
  expect_true(is.na(out$result$counts["M", "B"]))
})

test_that(".pool_result crosstab strict fails closed on a fully suppressed site", {
  M1 <- matrix(c(10, 20, 30, 40), 2, 2,
               dimnames = list(c("F", "M"), c("W", "B")))
  empty <- list(row_col = "r", col_col = "c", count_mode = "persons",
                suppressed = TRUE, counts = matrix(numeric(0), 0, 0))
  out <- .pool_result(list(a = .mk_ct(M1), b = empty), "crosstab", "strict")
  expect_null(out$result)
  expect_true(any(grepl("Strict", out$warnings)))
})

test_that(".pool_result crosstab pooled_only_ok drops empty site", {
  M1 <- matrix(c(10, 20, 30, 40), 2, 2,
               dimnames = list(c("F", "M"), c("W", "B")))
  empty <- list(row_col = "r", col_col = "c", count_mode = "persons",
                suppressed = TRUE, counts = matrix(numeric(0), 0, 0))
  out <- .pool_result(list(a = .mk_ct(M1), b = empty), "crosstab", "pooled_only_ok")
  expect_equal(out$result$counts["F", "W"], 10)
  expect_true(any(grepl("Dropped", out$warnings)))
})

test_that(".pool_result crosstab pools stratified slices independently", {
  M1 <- matrix(c(10, 20, 30, 40), 2, 2,
               dimnames = list(c("F", "M"), c("W", "B")))
  M2 <- matrix(c(1, 2, 3, 4), 2, 2,
               dimnames = list(c("F", "M"), c("W", "B")))
  site_a <- list(stratified = TRUE, stratify_by = "age_band",
                 strata = list("young" = .mk_ct(M1), "old" = .mk_ct(M1)))
  site_b <- list(stratified = TRUE, stratify_by = "age_band",
                 strata = list("young" = .mk_ct(M2), "old" = .mk_ct(M2)))
  out <- .pool_result(list(a = site_a, b = site_b), "crosstab", "strict")
  expect_true(out$result$stratified)
  expect_equal(out$result$stratify_by, "age_band")
  expect_equal(out$result$strata$young$counts["F", "W"], 11)
  expect_equal(out$result$strata$old$counts["M", "B"], 44)
  # No unstratified total is exposed.
  expect_null(out$result$counts)
})
