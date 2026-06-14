# ==============================================================================
# Tests for the client Unified Analysis Catalog wrappers (Phase 6a)
#
# These cover the three public wrappers' signatures, the internal call-builders
# that splice scope/combine into the server call (the security-relevant part:
# omop.table symbols travel UNEVALUATED so DataSHIELD resolves them server-side,
# and combine is always passed by NAME so a NULL scope can't shift it into the
# wrong positional slot), and the deprecated query-library shims that now forward
# to ds.omop.analysis.*.
# ==============================================================================

# --- Public signatures -------------------------------------------------------

test_that("ds.omop.analysis.list has the expected signature", {
  expect_true(is.function(ds.omop.analysis.list))
  args <- formals(ds.omop.analysis.list)
  expect_true(all(c("domain", "symbol", "conns") %in% names(args)))
  expect_null(args$domain)
  expect_equal(args$symbol, "omop")
})

test_that("ds.omop.analysis.get has the expected signature", {
  expect_true(is.function(ds.omop.analysis.get))
  args <- formals(ds.omop.analysis.get)
  expect_true(all(c("name", "symbol", "conns") %in% names(args)))
  expect_equal(args$symbol, "omop")
})

test_that("ds.omop.analysis.run has the expected signature", {
  expect_true(is.function(ds.omop.analysis.run))
  args <- formals(ds.omop.analysis.run)
  expect_true(all(c("name", "params", "cohort", "tables", "combine",
                    "pooling_policy", "symbol", "conns") %in% names(args)))
  expect_equal(args$combine, "union")
  expect_equal(args$pooling_policy, "strict")
})

# --- .query_id_to_name -------------------------------------------------------

test_that(".query_id_to_name prefixes a bare legacy id", {
  expect_equal(dsOMOPClient:::.query_id_to_name("condition_prevalence"),
               "dsomop:condition_prevalence")
})

test_that(".query_id_to_name leaves an already-prefixed name untouched", {
  expect_equal(dsOMOPClient:::.query_id_to_name("dsomop:achilles.401"),
               "dsomop:achilles.401")
})

# --- .analysis_scope_expr ----------------------------------------------------

test_that(".analysis_scope_expr: NULL cohort + NULL tables -> NULL", {
  expect_null(dsOMOPClient:::.analysis_scope_expr(NULL, NULL))
})

test_that(".analysis_scope_expr: cohort-only passes the literal through", {
  # A cohort definition id is coerced to an integer literal (not a call).
  expr <- dsOMOPClient:::.analysis_scope_expr(cohort = 1L, tables = NULL)
  expect_false(is.call(expr))
  expect_equal(expr, 1L)

  # A cohort handle unwraps to its server-side table name.
  ch <- structure("dsomop_cohort_7", class = "dsomop_cohort_handle")
  expect_equal(dsOMOPClient:::.analysis_scope_expr(cohort = ch, tables = NULL),
               "dsomop_cohort_7")
})

test_that(".analysis_scope_expr: table symbols become an unevaluated list() call", {
  expr <- dsOMOPClient:::.analysis_scope_expr(cohort = NULL,
                                              tables = c("tblA", "tblB"))
  expect_true(is.call(expr))
  # list(as.name("tblA"), as.name("tblB")) — symbols, NOT strings, so the server
  # resolves them to the live omop.table frames.
  expect_identical(expr[[1]], as.name("list"))
  expect_identical(expr[[2]], as.name("tblA"))
  expect_identical(expr[[3]], as.name("tblB"))
})

test_that(".analysis_scope_expr: cohort + tables mixes literal and symbols", {
  expr <- dsOMOPClient:::.analysis_scope_expr(cohort = 2L, tables = "tblA")
  expect_true(is.call(expr))
  expect_identical(expr[[1]], as.name("list"))
  expect_equal(expr[[2]], 2L)              # cohort literal first
  expect_identical(expr[[3]], as.name("tblA"))  # then the table symbol
})

test_that(".analysis_scope_expr: non-character tables is rejected", {
  expect_error(dsOMOPClient:::.analysis_scope_expr(NULL, tables = 123),
               "name\\(s\\) of server-side omop.table")
})

# --- .analysis_run_call ------------------------------------------------------

test_that(".analysis_run_call: no scope still passes combine by NAME", {
  call <- dsOMOPClient:::.analysis_run_call(
    "omopAnalysisRunDS", "res", "dsomop:achilles.401",
    params = list(), scope_expr = NULL, combine = "union")
  expect_true(is.call(call))
  expect_identical(call[[1]], as.name("omopAnalysisRunDS"))
  # combine is a NAMED argument (never positional), so a NULL scope cannot shift
  # it into the scope slot.
  expect_equal(call$combine, "union")
  expect_null(call$scope)
})

test_that(".analysis_run_call: scope is spliced in by NAME", {
  scope_expr <- dsOMOPClient:::.analysis_scope_expr(NULL, tables = "tblA")
  call <- dsOMOPClient:::.analysis_run_call(
    "omopAnalysisRunDS", "res", "dsomop:condition.prevalence_by_concept",
    params = list(top_n = 25), scope_expr = scope_expr, combine = "intersect")
  expect_true(is.call(call))
  # The scope argument is named and carries the unevaluated list() of symbols.
  expect_true(is.call(call$scope))
  expect_identical(call$scope[[2]], as.name("tblA"))
  expect_equal(call$combine, "intersect")
  # params are base64/JSON-encoded for transport (a positional arg, not raw).
  encoded <- call[[4]]
  expect_true(is.character(encoded))
})

test_that(".analysis_run_call: params survive .ds_encode round-trip slot", {
  call <- dsOMOPClient:::.analysis_run_call(
    "omopAnalysisRunDS", "res", "dsomop:x", params = list(a = 1, b = 2),
    scope_expr = NULL, combine = "union")
  # The 4th positional element is the encoded params payload.
  expect_equal(call[[4]], dsOMOPClient:::.ds_encode(list(a = 1, b = 2)))
})

# --- Deprecated query-library shims ------------------------------------------

test_that("query-library wrappers are retained as deprecated shims", {
  for (fn in c("ds.omop.query.list", "ds.omop.query.get",
               "ds.omop.query.exec")) {
    expect_true(is.function(get(fn)),
                info = paste(fn, "should still exist for back-compat"))
  }
})

test_that("ds.omop.query.list warns (deprecated) and forwards", {
  # No active session here, so the forward will error AFTER the deprecation
  # warning fires; we assert the warning specifically.
  expect_warning(
    tryCatch(ds.omop.query.list(), error = function(e) NULL),
    "deprecated|ds.omop.analysis.list", ignore.case = TRUE)
})

test_that("ds.omop.query.get warns (deprecated) and forwards", {
  expect_warning(
    tryCatch(ds.omop.query.get("condition_prevalence"),
             error = function(e) NULL),
    "deprecated|ds.omop.analysis.get", ignore.case = TRUE)
})

test_that("ds.omop.query.exec warns (deprecated) and forwards", {
  expect_warning(
    tryCatch(ds.omop.query.exec("condition_prevalence"),
             error = function(e) NULL),
    "deprecated|ds.omop.analysis.run", ignore.case = TRUE)
})
