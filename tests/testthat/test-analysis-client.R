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

test_that("ds.omop.prevalence / ds.omop.distribution are thin one-liners", {
  expect_true(is.function(ds.omop.prevalence))
  pa <- formals(ds.omop.prevalence)
  expect_true(all(c("concept_id", "cohort", "domain", "top_n", "tables",
                    "symbol", "conns") %in% names(pa)))
  expect_equal(pa$domain, "condition")
  expect_true(is.function(ds.omop.distribution))
  da <- formals(ds.omop.distribution)
  expect_true(all(c("cohort", "metric", "domain", "top_n", "concept_id",
                    "tables", "symbol", "conns") %in% names(da)))
  expect_equal(da$metric, "measurement_value")
})

# --- .analysis_domain_code ---------------------------------------------------

test_that(".analysis_domain_code maps names and codes, rejects junk", {
  f <- dsOMOPClient:::.analysis_domain_code
  expect_equal(f("condition"), "0")
  expect_equal(f("drug"), "1")
  expect_equal(f("measurement"), "3")
  expect_equal(f("3"), "3")          # a code passes through
  expect_equal(f(NULL, default = "3"), "3")
  expect_error(f("nonsense"), "Unknown domain")
})

# --- .analysis_filter_concepts (post-gate row subset) ------------------------

test_that(".analysis_filter_concepts subsets pooled + per_site by concept id", {
  res <- dsomop_result(
    per_site = list(s = data.frame(covariate_id = c(1L, 2L, 3L),
                                   sum_value = c(5, 10, 15))),
    pooled = data.frame(covariate_id = c(1L, 2L, 3L), sum_value = c(5, 10, 15)))
  out <- dsOMOPClient:::.analysis_filter_concepts(res, concept_id = 2L)
  expect_equal(out$pooled$covariate_id, 2L)
  expect_equal(out$per_site$s$covariate_id, 2L)
  # NULL concept_id is a no-op (returns all rows untouched).
  same <- dsOMOPClient:::.analysis_filter_concepts(res, concept_id = NULL)
  expect_equal(nrow(same$pooled), 3L)
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

# ==============================================================================
# Phase 6b: client-side plot harness (ds.omop.analysis.run plot=)
#
# The plotting half runs ENTIRELY on the client over data that already cleared
# the server's single per-patient gate. The server ships an INERT plot recipe in
# the entry metadata (plot$code = the SOURCE TEXT of a function(df, params)); the
# client eval()s that text LOCALLY and calls it on the pooled gated frame. The
# load-bearing security property: the client NEVER sends plot code to the server
# and the server NEVER evals it. These tests pin (1) the .analysis_render_plot
# helper's branches directly, and (2) the end-to-end run path with DataSHIELD
# mocked so we can both prove plot=FALSE never touches the recipe AND capture
# every server-bound expression to prove no code is ever transmitted.
# ==============================================================================

# A trivial, valid plot recipe: source text of a function(df, params) -> ggplot.
.acat_plot_code <- paste(
  "function(df, params) {",
  "  ggplot2::ggplot(df, ggplot2::aes(x = gender_name, y = n_persons)) +",
  "    ggplot2::geom_col()",
  "}",
  sep = "\n")

# A small ALREADY-GATED pooled frame (banded counts), as the gate would emit.
.acat_gated_df <- function() {
  data.frame(gender_name = c("MALE", "FEMALE"),
             n_persons = c(45, 30), stringsAsFactors = FALSE)
}

# --- .analysis_render_plot (unit, no session) --------------------------------

test_that(".analysis_render_plot builds a ggplot from an inert recipe on gated df", {
  skip_if_not_installed("ggplot2")
  meta <- list(name = "dsomop:demo.person_count_by_gender",
               plot = list(type = "bar", code = .acat_plot_code))
  p <- dsOMOPClient:::.analysis_render_plot(meta, .acat_gated_df(),
                                            params = list())
  expect_s3_class(p, "ggplot")
  # The recipe was evaluated over the GATED frame we passed (not some other data):
  # the built plot's data is exactly that banded frame.
  expect_equal(p$data$n_persons, c(45, 30))
})

test_that(".analysis_render_plot reads a nested compute$plot recipe too", {
  skip_if_not_installed("ggplot2")
  meta <- list(name = "x", compute = list(plot = list(code = .acat_plot_code)))
  p <- dsOMOPClient:::.analysis_render_plot(meta, .acat_gated_df(), list())
  expect_s3_class(p, "ggplot")
})

test_that(".analysis_render_plot returns NULL (warns) when no recipe is shipped", {
  meta <- list(name = "dsomop:achilles.401")  # no plot field
  expect_warning(
    p <- dsOMOPClient:::.analysis_render_plot(meta, .acat_gated_df(), list()),
    "does not provide a plot")
  expect_null(p)
})

test_that(".analysis_render_plot degrades to NULL+warn on a broken recipe (data kept)", {
  skip_if_not_installed("ggplot2")
  # A recipe that errors when called must NOT propagate: the caller already holds
  # the data, so plotting failure is a warning + NULL plot, never a hard error.
  meta <- list(name = "x", plot = list(code = "function(df, params) stop('boom')"))
  expect_warning(
    p <- dsOMOPClient:::.analysis_render_plot(meta, .acat_gated_df(), list()),
    "plot could not be built")
  expect_null(p)
})

test_that(".analysis_render_plot rejects a recipe that isn't a ggplot", {
  skip_if_not_installed("ggplot2")
  meta <- list(name = "x", plot = list(code = "function(df, params) 42"))
  expect_warning(
    p <- dsOMOPClient:::.analysis_render_plot(meta, .acat_gated_df(), list()),
    "plot could not be built")
  expect_null(p)
})

# --- ds.omop.analysis.run(plot=) end-to-end (mocked DataSHIELD) ---------------

# Register a fake session AND mock DSI::datashield.aggregate so the run path can
# execute with no live backend. The mock dispatches on the call HEAD:
#   omopAnalysisGetDS -> the entry metadata (carrying the inert plot recipe)
#   omopAnalysisRunDS -> the already-gated aggregate frame
# Every expression handed to the server is recorded in `sent` so a test can
# assert no plot code was ever transmitted.
.acat_with_mocked_run <- function(meta_plot, gated, code) {
  assign("omop", list(conns = list(srv = "FAKE"), res_symbol = "dsO.fake"),
         envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists("omop", envir = dsOMOPClient:::.dsomop_client_env)) {
      rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env)
    })

  sent <- new.env(parent = emptyenv())
  sent$exprs <- list()
  meta <- list(name = "dsomop:demo.person_count_by_gender", mode = "aggregate")
  if (!is.null(meta_plot)) meta$plot <- meta_plot

  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      sent$exprs <- c(sent$exprs, list(expr))
      head <- if (is.call(expr)) as.character(expr[[1]]) else ""
      val <- if (identical(head, "omopAnalysisGetDS")) meta else gated
      stats::setNames(list(val), names(conns))
    },
    .package = "DSI", .env = parent.frame())

  code(sent)
}

test_that("ds.omop.analysis.run(plot=FALSE) returns data only, no plot, no ggplot", {
  gated <- .acat_gated_df()
  .acat_with_mocked_run(
    meta_plot = list(type = "bar", code = .acat_plot_code),
    gated = gated,
    code = function(sent) {
      res <- ds.omop.analysis.run("dsomop:demo.person_count_by_gender",
                                  plot = FALSE)
      # Data is returned (pooled = the gated frame, one server). Pooling groups
      # by gender_name so row order is not input order; key on the label.
      expect_s3_class(res, "dsomop_result")
      pn <- stats::setNames(res$pooled$n_persons, res$pooled$gender_name)
      expect_equal(pn[["MALE"]], 45)
      expect_equal(pn[["FEMALE"]], 30)
      # No plot is attached on the default path.
      expect_null(attr(res, "plot"))
      expect_null(res$meta$plot)
    })
})

test_that("ds.omop.analysis.run(plot=TRUE) evals the recipe CLIENT-SIDE on the gated df", {
  skip_if_not_installed("ggplot2")
  gated <- .acat_gated_df()
  .acat_with_mocked_run(
    meta_plot = list(type = "bar", code = .acat_plot_code),
    gated = gated,
    code = function(sent) {
      res <- ds.omop.analysis.run("dsomop:demo.person_count_by_gender",
                                  plot = TRUE)
      gg <- attr(res, "plot")
      expect_s3_class(gg, "ggplot")
      # The plot was drawn over the GATED, banded data — not raw counts. The
      # plot's data IS the pooled gate-passed frame (key on the gender label,
      # since pooling reorders rows).
      ggn <- stats::setNames(gg$data$n_persons, gg$data$gender_name)
      expect_equal(ggn[["MALE"]], 45)
      expect_equal(ggn[["FEMALE"]], 30)
      # The data is still returned intact alongside the plot.
      pn <- stats::setNames(res$pooled$n_persons, res$pooled$gender_name)
      expect_equal(pn[["MALE"]], 45)
      # meta$plot and the attribute are the same built ggplot.
      expect_identical(res$meta$plot, gg)
    })
})

test_that("plot recipe is NEVER sent to the server (client-side eval only)", {
  skip_if_not_installed("ggplot2")
  gated <- .acat_gated_df()
  .acat_with_mocked_run(
    meta_plot = list(type = "bar", code = .acat_plot_code),
    gated = gated,
    code = function(sent) {
      ds.omop.analysis.run("dsomop:demo.person_count_by_gender", plot = TRUE)
      # Every server-bound expression is a known catalog method, never the plot
      # code. Deparse each and assert none carries the recipe source text.
      heads <- vapply(sent$exprs,
                      function(e) as.character(e[[1]]), character(1))
      expect_true(all(heads %in% c("omopAnalysisGetDS", "omopAnalysisRunDS")))
      depars <- vapply(sent$exprs,
                       function(e) paste(deparse(e), collapse = " "), character(1))
      expect_false(any(grepl("geom_col", depars, fixed = TRUE)))
      expect_false(any(grepl("ggplot", depars, fixed = TRUE)))
    })
})

test_that("ds.omop.analysis.run(plot=TRUE) on an entry with no recipe keeps the data", {
  skip_if_not_installed("ggplot2")
  gated <- .acat_gated_df()
  .acat_with_mocked_run(
    meta_plot = NULL,  # entry ships no plot recipe
    gated = gated,
    code = function(sent) {
      expect_warning(
        res <- ds.omop.analysis.run("dsomop:demo.person_count_by_gender",
                                    plot = TRUE),
        "does not provide a plot")
      expect_null(attr(res, "plot"))
      # Data is never lost when a plot can't be built.
      pn <- stats::setNames(res$pooled$n_persons, res$pooled$gender_name)
      expect_equal(pn[["MALE"]], 45)
    })
})

# --- One-liner delegation (mocked DataSHIELD) --------------------------------
#
# Prove ds.omop.prevalence / ds.omop.distribution are thin: they delegate to the
# server's fe.prevalence / fe.continuous entries with the mapped params, and the
# concept_id post-filter subsets the already-gated frame.

# A gated covariate frame as fe.prevalence/fe.continuous would emit.
.acat_cov_df <- function() {
  data.frame(covariate_id = c(201820L, 320128L),
             covariate_name = c("Diabetes", "Hypertension"),
             sum_value = c(45, 30), average = c(0.9, 0.6),
             stringsAsFactors = FALSE)
}

test_that("ds.omop.prevalence delegates to fe.prevalence with mapped params", {
  .acat_with_mocked_run(
    meta_plot = NULL, gated = .acat_cov_df(),
    code = function(sent) {
      res <- ds.omop.prevalence(cohort = 1L, domain = "drug", top_n = 10)
      expect_s3_class(res, "dsomop_result")
      run <- Filter(function(e) identical(as.character(e[[1]]), "omopAnalysisRunDS"),
                    sent$exprs)[[1]]
      # entry name (3rd positional) is the fe.prevalence catalog id.
      expect_equal(run[[3]], "dsomop:fe.prevalence")
      # params (4th positional) carry the mapped domain_code + top_n.
      params <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(
        gsub("_", "/", gsub("-", "+", sub("^B64:", "", run[[4]]))))))
      expect_equal(params$domain_code, "1")   # "drug" -> "1"
      expect_equal(params$top_n, 10)
    })
})

test_that("ds.omop.prevalence(concept_id=) narrows the gated result", {
  .acat_with_mocked_run(
    meta_plot = NULL, gated = .acat_cov_df(),
    code = function(sent) {
      res <- ds.omop.prevalence(concept_id = 201820, cohort = 1L)
      expect_equal(res$pooled$covariate_id, 201820L)
      expect_equal(nrow(res$pooled), 1L)
    })
})

test_that("ds.omop.distribution delegates to fe.continuous with the metric", {
  .acat_with_mocked_run(
    meta_plot = NULL, gated = .acat_cov_df(),
    code = function(sent) {
      ds.omop.distribution(cohort = 1L, metric = "age")
      run <- Filter(function(e) identical(as.character(e[[1]]), "omopAnalysisRunDS"),
                    sent$exprs)[[1]]
      expect_equal(run[[3]], "dsomop:fe.continuous")
      params <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(
        gsub("_", "/", gsub("-", "+", sub("^B64:", "", run[[4]]))))))
      expect_equal(params$metric, "age")
    })
})
