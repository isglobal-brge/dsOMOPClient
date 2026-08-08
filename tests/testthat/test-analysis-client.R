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

test_that(".analysis_scope_expr: cohort-only stays literal in named scope", {
  # A cohort definition id is coerced to an integer literal, never a call.
  args <- dsOMOPClient:::.analysis_scope_expr(cohort = 1L, tables = NULL)
  expect_named(args, "scope")
  expect_false(is.call(args$scope))
  expect_equal(args$scope, 1L)

  # A cohort handle unwraps to its server-side table name.
  ch <- structure("dsomop_cohort_7", class = "dsomop_cohort_handle")
  args <- dsOMOPClient:::.analysis_scope_expr(cohort = ch, tables = NULL)
  expect_equal(args$scope, "dsomop_cohort_7")
})

test_that(".analysis_scope_expr: tables become separate named bare symbols", {
  args <- dsOMOPClient:::.analysis_scope_expr(
    cohort = NULL, tables = c("tblA", "tblB")
  )
  expect_named(args, c("scope_table_1", "scope_table_2"))
  expect_identical(args$scope_table_1, as.name("tblA"))
  expect_identical(args$scope_table_2, as.name("tblB"))
  expect_false(any(vapply(args, is.call, logical(1L))))
})

test_that(".analysis_scope_expr: cohort and tables remain separate arguments", {
  args <- dsOMOPClient:::.analysis_scope_expr(cohort = 2L, tables = "tblA")
  expect_named(args, c("scope", "scope_table_1"))
  expect_equal(args$scope, 2L)
  expect_identical(args$scope_table_1, as.name("tblA"))
})

test_that(".analysis_scope_expr: cohort vectors use scalar top-level args", {
  args <- dsOMOPClient:::.analysis_scope_expr(
    cohort = c(1L, 2L), tables = "tblA"
  )
  expect_named(args, c("scope_cohort_1", "scope_cohort_2", "scope_table_1"))
  expect_identical(args$scope_cohort_1, 1L)
  expect_identical(args$scope_cohort_2, 2L)
  expect_identical(args$scope_table_1, as.name("tblA"))

  call <- dsOMOPClient:::.analysis_run_call(
    "omopAnalysisRunDS", "res", "dsomop:cm.effect_estimate",
    params = list(), scope_args = args, combine = "union"
  )
  expect_identical(call$scope_cohort_1, 1L)
  expect_identical(call$scope_cohort_2, 2L)
  expect_false(any(vapply(as.list(call)[-1L], is.call, logical(1L))))
  expect_false(grepl("(^|[^[:alnum:]_.])(c|list)\\s*\\(|1:2",
                     paste(deparse(call), collapse = " ")))

  handle <- structure("target_table", class = "dsomop_cohort_handle")
  mixed <- dsOMOPClient:::.analysis_scope_expr(
    cohort = list(handle, 2L), tables = NULL
  )
  expect_identical(mixed$scope_cohort_1, "target_table")
  expect_identical(mixed$scope_cohort_2, 2L)
  expect_error(
    dsOMOPClient:::.analysis_scope_expr(
      cohort = c(unclass(handle), 2L), tables = NULL
    ),
    "ambiguous.*use list"
  )
})

test_that(".analysis_scope_expr: non-character tables is rejected", {
  expect_error(dsOMOPClient:::.analysis_scope_expr(NULL, tables = 123),
               "name\\(s\\) of server-side omop.table")
  expect_error(
    dsOMOPClient:::.analysis_scope_expr(NULL, tables = "list(x)"),
    "name\\(s\\) of server-side omop.table"
  )
  expect_error(
    dsOMOPClient:::.analysis_scope_expr(NULL, tables = c("tblA", "tblA")),
    "name\\(s\\) of server-side omop.table"
  )
  expect_error(
    dsOMOPClient:::.analysis_scope_expr(
      cohort = call("list", as.name("secret")), tables = NULL
    ),
    "one or more literal cohort ids or table names"
  )
  expect_error(
    dsOMOPClient:::.analysis_scope_expr(cohort = c(1, 2.5), tables = NULL),
    "positive finite integer-like"
  )
  expect_error(dsOMOPClient:::.analysis_scope_expr(cohort = 0L),
               "positive finite integer-like")
  expect_error(dsOMOPClient:::.analysis_scope_expr(cohort = "-1"),
               "positive finite integer-like")
})

# --- .analysis_run_call ------------------------------------------------------

test_that(".analysis_run_call: no scope still passes combine by NAME", {
  call <- dsOMOPClient:::.analysis_run_call(
    "omopAnalysisRunDS", "res", "dsomop:achilles.401",
    params = list(), scope_args = NULL, combine = "union")
  expect_true(is.call(call))
  expect_identical(call[[1]], as.name("omopAnalysisRunDS"))
  # combine is a NAMED argument (never positional), so a NULL scope cannot shift
  # it into the scope slot.
  expect_equal(call$combine, "union")
  expect_null(call$scope)
})

test_that(".analysis_run_call: scope arguments are spliced without list/c", {
  scope_args <- dsOMOPClient:::.analysis_scope_expr(
    cohort = 7L, tables = c("tblA", "tblB")
  )
  call <- dsOMOPClient:::.analysis_run_call(
    "omopAnalysisRunDS", "res", "dsomop:condition.prevalence_by_concept",
    params = list(top_n = 25), scope_args = scope_args, combine = "intersect")
  expect_true(is.call(call))
  expect_equal(call$scope, 7L)
  expect_identical(call$scope_table_1, as.name("tblA"))
  expect_identical(call$scope_table_2, as.name("tblB"))
  expect_equal(call$combine, "intersect")
  nested_heads <- vapply(as.list(call)[-1L], function(value) {
    if (is.call(value)) as.character(value[[1L]]) else ""
  }, character(1L))
  expect_false(any(nested_heads %in% c("c", "list")))
  # params are base64/JSON-encoded for transport (a positional arg, not raw).
  encoded <- call[[4]]
  expect_true(is.character(encoded))
})

test_that(".analysis_run_call rejects arbitrary nested scope expressions", {
  expect_error(
    dsOMOPClient:::.analysis_run_call(
      "omopAnalysisRunDS", "res", "dsomop:x", params = list(),
      scope_args = list(scope_table_1 = call("list", as.name("secret"))),
      combine = "union"
    ),
    "sequential bare"
  )
})

test_that(".analysis_run_call: params survive .ds_encode round-trip slot", {
  call <- dsOMOPClient:::.analysis_run_call(
    "omopAnalysisRunDS", "res", "dsomop:x", params = list(a = 1, b = 2),
    scope_args = NULL, combine = "union")
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

test_that("ds.omop.query.exec fails closed for caller-selected assign mode", {
  expect_warning(
    expect_error(
      ds.omop.query.exec("condition_prevalence", mode = "assign"),
      "no longer accepted"
    ),
    "deprecated|ds.omop.analysis.run", ignore.case = TRUE
  )
})

# ==============================================================================
# Phase 6b: client-side plot harness (ds.omop.analysis.run plot=)
#
# The plotting half runs ENTIRELY on the client over data that already cleared
# the server's single per-patient gate. The server may ship declarative metadata
# (an allowlisted type plus column mappings); source text is ignored and never
# parsed/evaluated. These tests pin (1) the .analysis_render_plot
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

test_that(".analysis_render_plot builds a ggplot from declarative metadata", {
  skip_if_not_installed("ggplot2")
  meta <- list(name = "dsomop:demo.person_count_by_gender",
               plot = list(type = "bar",
                           mapping = list(x = "gender_name", y = "n_persons"),
                           code = .acat_plot_code))
  p <- dsOMOPClient:::.analysis_render_plot(meta, .acat_gated_df(),
                                            params = list())
  expect_s3_class(p, "ggplot")
  # The recipe was evaluated over the GATED frame we passed (not some other data):
  # the built plot's data is exactly that banded frame.
  expect_equal(p$data$n_persons, c(45, 30))
})

test_that(".analysis_render_plot reads a nested compute$plot recipe too", {
  skip_if_not_installed("ggplot2")
  meta <- list(name = "x", compute = list(plot = list(
    type = "bar", mapping = list(x = "gender_name", y = "n_persons"),
    code = .acat_plot_code)))
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

test_that(".analysis_render_plot ignores hostile source text", {
  skip_if_not_installed("ggplot2")
  touched <- FALSE
  meta <- list(name = "x", plot = list(
    type = "bar", mapping = list(x = "gender_name", y = "n_persons"),
    code = "{ touched <<- TRUE; system('false') }"))
  p <- dsOMOPClient:::.analysis_render_plot(meta, .acat_gated_df(), list())
  expect_s3_class(p, "ggplot")
  expect_false(touched)
})

test_that(".analysis_render_plot rejects unsupported plot types", {
  skip_if_not_installed("ggplot2")
  meta <- list(name = "x", plot = list(type = "arbitrary_code",
                                        code = "function(df, params) 42"))
  expect_warning(
    p <- dsOMOPClient:::.analysis_render_plot(meta, .acat_gated_df(), list()),
    "unsupported plot type")
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

test_that("analysis table scopes cross as separate named symbol arguments", {
  .acat_with_mocked_run(
    meta_plot = NULL,
    gated = .acat_gated_df(),
    code = function(sent) {
      ds.omop.analysis.run(
        "dsomop:demo.person_count_by_gender",
        cohort = 9L, tables = c("eligible_a", "eligible_b"),
        combine = "intersect"
      )
      run <- Filter(function(expr) {
        identical(as.character(expr[[1L]]), "omopAnalysisRunDS")
      }, sent$exprs)[[1L]]
      args <- as.list(run)
      expect_identical(args$scope, 9L)
      expect_identical(args$scope_table_1, as.name("eligible_a"))
      expect_identical(args$scope_table_2, as.name("eligible_b"))
      expect_identical(args$combine, "intersect")
      nested_heads <- vapply(args[-1L], function(value) {
        if (is.call(value)) as.character(value[[1L]]) else ""
      }, character(1L))
      expect_false(any(nested_heads %in% c("c", "list")))
    })
})

test_that("ds.omop.analysis.run(plot=TRUE) renders declaratively on gated df", {
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

.acat_test_session <- function(conns, outputs = NULL, scope_cap = 8L) {
  disclosure <- list(
    harmonization_contract_version = "dsomop-harmonization-v3",
    age_breaks = seq(0, 85, 5),
    age_semantics = "reference_year_minus_year_of_birth",
    date_semantics = "ISO8601_Gregorian_closed_interval",
    date_granularity = "calendar_day",
    datetime_timezone = "UTC", week_start = "Monday",
    nfilter_age_range = 5, nfilter_date_range = 30, nfilter_band = 5,
    max_feature_specs = 1000, max_pivot_concepts = 1000,
    max_output_columns = 5000, max_temporal_bins = 10000,
    max_events_per_group = 100,
    max_filter_depth = 32, max_filter_nodes = 1024,
    max_filter_values = 10000, max_plan_outputs = 100,
    max_analysis_scope_tables = scope_cap
  )
  list(
    conns = conns, res_symbol = "dsO.fake", outputs = outputs,
    capabilities = stats::setNames(
      rep(list(list(disclosure = disclosure)), length(conns)), names(conns)
    )
  )
}

test_that("analysis scope obeys negotiated table and total-source caps", {
  conns <- list(a = "A", b = "B")
  assign("omop", .acat_test_session(conns, scope_cap = 1L),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  contacted <- FALSE
  local_mocked_bindings(
    datashield.aggregate = function(...) {
      contacted <<- TRUE
      stop("metadata should not be contacted", call. = FALSE)
    },
    .package = "DSI"
  )

  expect_error(
    ds.omop.analysis.run("dsomop:x", tables = c("scope_a", "scope_b")),
    "max_analysis_scope_tables cap of 1"
  )
  expect_error(
    ds.omop.analysis.run("dsomop:x", cohort = c(1L, 2L, 3L)),
    "total source cap of 2"
  )
  expect_false(contacted)
})

test_that("analysis execution rejects inconsistent cross-server metadata", {
  conns <- list(a = "A", b = "B")
  assign("omop", .acat_test_session(conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)

  local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      mode <- if (identical(server, "a")) "aggregate" else "assign"
      stats::setNames(list(list(name = "dsomop:x", mode = mode)), server)
    },
    .package = "DSI"
  )
  expect_error(ds.omop.analysis.run("dsomop:x"),
               "differs across servers")
})

test_that("aggregate analysis never publishes a partial federation", {
  conns <- list(a = "A", b = "B")
  assign("omop", .acat_test_session(conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)

  local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      head <- if (is.call(expr)) as.character(expr[[1L]]) else ""
      if (identical(head, "omopAnalysisGetDS")) {
        return(stats::setNames(
          list(list(name = "dsomop:x", mode = "aggregate")), server
        ))
      }
      if (identical(server, "b")) stop("simulated analysis failure")
      stats::setNames(list(data.frame(n_persons = 10)), server)
    },
    .package = "DSI"
  )

  expect_error(
    ds.omop.analysis.run("dsomop:x"),
    "Partial-site analysis results are not published"
  )
})

test_that("partial assign-mode analysis is removed from every server", {
  conns <- list(a = "A", b = "B")
  assign("omop", .acat_test_session(conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  symbols <- list(a = character(0), b = character(0))
  assigned <- NULL

  local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      stats::setNames(list(list(name = "dsomop:loader", mode = "assign")),
                      server)
    },
    datashield.symbols = function(conns, ...) symbols[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      assigned <<- symbol
      symbols$a <<- union(symbols$a, symbol)
      success("a")
      error("b", "simulated failure")
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      symbols[[server]] <<- setdiff(symbols[[server]], symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )

  expect_error(ds.omop.analysis.run("dsomop:loader"), "rolled back")
  expect_true(is.character(assigned) && length(assigned) == 1L)
  expect_false(any(vapply(symbols, function(x) assigned %in% x, logical(1))))
})

test_that("assign-mode analysis commits only after every server owns the symbol", {
  conns <- list(a = "A", b = "B")
  assign("omop", .acat_test_session(conns, outputs = NULL),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  symbols <- list(a = character(0), b = character(0))

  local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      stats::setNames(list(list(name = "dsomop:loader", mode = "assign")),
                      server)
    },
    datashield.symbols = function(conns, ...) symbols[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      for (server in names(conns)) {
        symbols[[server]] <<- union(symbols[[server]], symbol)
        success(server)
      }
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      symbols[[server]] <<- setdiff(symbols[[server]], symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )

  result <- ds.omop.analysis.run("dsomop:loader")
  assigned <- result$meta$assign_symbol
  expect_true(all(vapply(symbols, function(x) assigned %in% x, logical(1))))
  expect_identical(result$per_site, list(a = TRUE, b = TRUE))
  session <- get("omop", envir = dsOMOPClient:::.dsomop_client_env)
  expect_true(assigned %in% session$outputs)
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

# --- ds.omop.meta.effect_estimate (cross-site evidence synthesis) ------------

test_that("ds.omop.meta.effect_estimate has the expected signature", {
  expect_true(is.function(ds.omop.meta.effect_estimate))
  args <- formals(ds.omop.meta.effect_estimate)
  expect_true(all(c("name", "params", "cohort", "tables", "combine",
                    "pooling_policy", "symbol", "conns") %in% names(args)))
  expect_equal(args$name, "dsomop:cm.effect_estimate")
  expect_equal(args$pooling_policy, "strict")
})

# Multi-server mock: each server returns its OWN per-site effect-estimate frame,
# so the wrapper's inverse-variance re-pool produces a genuine 2-site meta.
.acat_with_mocked_meta <- function(per_server_frames, code) {
  conns <- stats::setNames(as.list(rep("FAKE", length(per_server_frames))),
                           names(per_server_frames))
  assign("omop", .acat_test_session(conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists("omop", envir = dsOMOPClient:::.dsomop_client_env)) {
      rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env)
    })
  meta <- list(name = "dsomop:cm.effect_estimate", mode = "aggregate")
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      head <- if (is.call(expr)) as.character(expr[[1]]) else ""
      if (identical(head, "omopAnalysisGetDS")) {
        return(stats::setNames(rep(list(meta), length(conns)), names(conns)))
      }
      stats::setNames(per_server_frames[names(conns)], names(conns))
    },
    .package = "DSI", .env = parent.frame())
  code()
}

test_that("ds.omop.meta.effect_estimate inverse-variance pools per-site HRs", {
  mk <- function(le, se) data.frame(
    arm = c("target", "comparator"),
    log_estimate = le, se_log_estimate = se, stringsAsFactors = FALSE)
  frames <- list(nairobi = mk(log(1.5), 0.2), douala = mk(log(2.0), 0.3))
  .acat_with_mocked_meta(frames, function() {
    res <- ds.omop.meta.effect_estimate(
      params = list(outcome_concept_id = 4329847), cohort = c(1L, 2L))
    expect_s3_class(res, "dsomop_result")
    # Pooled is the one-row meta-analysis, not the stacked per-arm frame.
    expect_equal(nrow(res$pooled), 1L)
    expect_equal(res$pooled$n_databases, 2L)
    w <- 1 / c(0.2, 0.3)^2
    fe <- sum(w * c(log(1.5), log(2.0))) / sum(w)
    expect_equal(res$pooled$estimate_fixed, exp(fe), tolerance = 1e-8)
    # Per-site frames are retained intact.
    expect_named(res$per_site, c("nairobi", "douala"))
  })
})

test_that("ds.omop.meta.effect_estimate strict fails closed if a site suppressed", {
  mk <- function(le, se) data.frame(
    log_estimate = le, se_log_estimate = se, stringsAsFactors = FALSE)
  # douala suppressed its estimate (small arm -> NA).
  frames <- list(nairobi = mk(log(1.5), 0.2),
                 douala = mk(NA_real_, NA_real_))
  .acat_with_mocked_meta(frames, function() {
    res <- ds.omop.meta.effect_estimate(cohort = c(1L, 2L),
                                        pooling_policy = "strict")
    expect_null(res$pooled)
    expect_true(any(grepl("Strict pooling failed", res$meta$warnings)))
  })
})
