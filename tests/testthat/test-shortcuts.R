# ==============================================================================
# Simplicity layer: the one-call shortcuts + the footgun fixes (client side).
#
# These assert the *client's* half of the simplicity work, with no live
# DataSHIELD backend (the server-side equality/gating proof lives in
# dsOMOP/tests/testthat/test-shortcuts-server.R, which executes against the
# SQLite fixture). Here we prove:
#   - "simple stays simple": ds.omop.prevalence / ds.omop.distribution are a
#     single call with sensible defaults, and they delegate to the right catalog
#     entry with the mapped params (the only thing they add over
#     ds.omop.analysis.run);
#   - the bare entry name travels through unchanged so the server resolves the
#     shorthand;
#   - a server-side requires-cohort error surfaces to the one-liner caller
#     (un-scoped fails closed, never a silent empty frame);
#   - footgun (a): omop_population/omop_variable_block accept a SINGLE filter and
#     it round-trips through codegen + circe;
#   - footgun (b): variables-without-output -> one wide default output;
#   - footgun (d): ds.omop.cohort.create auto-assigns a non-colliding id.
# ==============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# Decode a B64:-prefixed, url-safe base64 param blob (mirrors .ds_encode).
.shc_decode_params <- function(blob) {
  raw <- gsub("_", "/", gsub("-", "+", sub("^B64:", "", blob)))
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(raw)))
}

# Register a fake session + mock datashield.aggregate so the analysis run path
# executes offline. The mock dispatches on the call HEAD: metadata for
# omopAnalysisGetDS, the supplied `gated` frame for omopAnalysisRunDS. Every
# expression sent to the server is recorded so a test can assert the entry name
# and params. `run_error`, if set, makes omopAnalysisRunDS raise it (to prove a
# server-side error surfaces).
.shc_with_mocked_run <- function(gated, code, run_error = NULL) {
  assign("omop", list(conns = list(srv = "FAKE"), res_symbol = "dsO.fake"),
         envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists("omop", envir = dsOMOPClient:::.dsomop_client_env))
      rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env))

  sent <- new.env(parent = emptyenv())
  sent$exprs <- list()
  meta <- list(name = "dsomop:fe.prevalence", mode = "aggregate")

  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      sent$exprs <- c(sent$exprs, list(expr))
      head <- if (is.call(expr)) as.character(expr[[1]]) else ""
      if (identical(head, "omopAnalysisGetDS"))
        return(stats::setNames(list(meta), names(conns)))
      if (!is.null(run_error)) stop(run_error, call. = FALSE)
      stats::setNames(list(gated), names(conns))
    },
    .package = "DSI", .env = parent.frame())

  code(sent)
}

.shc_run_expr <- function(sent) {
  Filter(function(e) identical(as.character(e[[1]]), "omopAnalysisRunDS"),
         sent$exprs)[[1]]
}

.shc_cov_df <- function() {
  data.frame(covariate_id = c(201820L, 320128L),
             covariate_name = c("Diabetes", "Hypertension"),
             sum_value = c(45, 30), average = c(0.9, 0.6),
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# "SIMPLE STAYS SIMPLE": the shortcuts are one call with good defaults.
# ---------------------------------------------------------------------------

test_that("the prevalence/distribution shortcuts are a single call with defaults", {
  # Everything except the population is defaulted, so the simplest meaningful
  # invocation supplies only a cohort.
  pa <- formals(ds.omop.prevalence)
  expect_equal(pa$domain, "condition")
  expect_equal(pa$top_n, 50)
  expect_null(pa$concept_id)
  expect_false(isTRUE(pa$plot))            # plotting is opt-in
  expect_equal(pa$symbol, "omop")

  da <- formals(ds.omop.distribution)
  expect_equal(da$metric, "measurement_value")
  expect_equal(da$domain, "measurement")
  expect_equal(da$top_n, 50)

  # ds.omop.login: the single-server happy path needs only url/user/pw/resource;
  # everything else (server name, driver, symbol) is defaulted.
  la <- formals(ds.omop.login)
  expect_equal(la$driver, "OpalDriver")
  expect_equal(la$symbol, "omop")
  expect_null(la$server)
})

# ---------------------------------------------------------------------------
# Delegation: the shortcut adds only entry-name + param mapping over the run.
# ---------------------------------------------------------------------------

test_that("ds.omop.prevalence delegates to fe.prevalence (mapped domain + top_n)", {
  .shc_with_mocked_run(gated = .shc_cov_df(), code = function(sent) {
    res <- ds.omop.prevalence(cohort = 1L, domain = "drug", top_n = 10)
    expect_s3_class(res, "dsomop_result")
    run <- .shc_run_expr(sent)
    expect_equal(run[[3]], "dsomop:fe.prevalence")     # exact catalog id sent
    p <- .shc_decode_params(run[[4]])
    expect_equal(p$domain_code, "1")                   # "drug" -> "1"
    expect_equal(p$top_n, 10)
  })
})

test_that("ds.omop.distribution delegates to fe.continuous (metric carried)", {
  .shc_with_mocked_run(gated = .shc_cov_df(), code = function(sent) {
    ds.omop.distribution(cohort = 1L, metric = "age")
    run <- .shc_run_expr(sent)
    expect_equal(run[[3]], "dsomop:fe.continuous")
    p <- .shc_decode_params(run[[4]])
    expect_equal(p$metric, "age")
  })
})

test_that("ds.omop.prevalence(concept_id=) is a post-gate row subset", {
  .shc_with_mocked_run(gated = .shc_cov_df(), code = function(sent) {
    res <- ds.omop.prevalence(concept_id = 201820, cohort = 1L)
    expect_equal(nrow(res$pooled), 1L)
    expect_equal(res$pooled$covariate_id, 201820L)
  })
})

test_that("an un-scoped one-liner errors clearly client-side", {
  # The cohort-IS-the-population one-liners short-circuit with an immediate,
  # guiding error when no cohort/tables scope is given (better UX than a
  # captured server error in an empty result).
  expect_error(ds.omop.prevalence(), "computes prevalence WITHIN a cohort")
  expect_error(ds.omop.distribution(), "computes a distribution WITHIN a cohort")
})

test_that("an un-scoped requires-cohort error is preserved per-site (not dropped)", {
  # Via the POWER path (ds.omop.analysis.run, which does not pre-guard): the
  # server raises requires-cohort for a cohort-IS-the-population entry run
  # un-scoped. The client deliberately does NOT abort the whole federation on
  # one site's error (.ds_safe_aggregate captures per-server errors); the message
  # is preserved on the result's per_site ds_errors attribute and the pooled
  # frame is empty, never a silent fake-success row. (The hard, immediate error
  # is the SERVER-side contract, asserted in dsOMOP's catalog tests.)
  .shc_with_mocked_run(
    gated = .shc_cov_df(),
    run_error = "requires a cohort/population scope",
    code = function(sent) {
      res <- ds.omop.analysis.run("dsomop:fe.prevalence",
                                  params = list(domain_code = "0", top_n = 50L))
      errs <- attr(res$per_site, "ds_errors")
      expect_true(any(grepl("requires a cohort/population scope",
                            unlist(errs))))
      expect_true(is.null(res$pooled) || nrow(res$pooled) == 0L)
    })
})

# ---------------------------------------------------------------------------
# Footgun (a): a SINGLE filter / group is accepted and round-trips.
# ---------------------------------------------------------------------------

test_that("a single bare filter normalizes and survives codegen round-trip", {
  rec <- omop_recipe(
    populations = omop_population("g", filters = omop_filter_group(
      omop_filter_sex("F"),
      omop_filter_has_concept(201820, "condition_occurrence"),
      operator = "AND")),
    variables = omop_variable_age(),
    outputs = omop_output(name = "o", type = "wide", population_id = "g"))

  # circe export no longer hits the "$ operator is invalid for atomic vectors".
  expect_silent(circe <- recipe_export_circe(rec, population_id = "g"))
  expect_type(circe, "character")

  # The generated code re-evaluates to an equivalent recipe with the filter
  # preserved as a one-element list.
  code <- recipe_to_code(rec)
  rebuilt <- eval(parse(text = paste(code, collapse = "\n")))
  expect_s3_class(rebuilt, "omop_recipe")
  expect_length(rebuilt$populations$g$filters, 1L)
})

test_that("a single bare row filter on a block normalizes to a list", {
  blk <- omop_variable_block(table = "measurement", concept_ids = 3004410L,
                             filters = omop_filter_date_range(start = "2010-01-01"))
  expect_true(is.list(blk$filters) && !inherits(blk$filters, "omop_filter"))
  expect_length(blk$filters, 1L)
})

# ---------------------------------------------------------------------------
# Footgun (b): variables but no output -> exactly one wide default output.
# ---------------------------------------------------------------------------

test_that("variables-without-output compiles to one default wide output", {
  r <- omop_recipe(variables = omop_variable_age())
  expect_length(r$outputs, 0L)                 # author declared none
  plan <- recipe_to_plan(r)
  expect_length(plan$outputs, 1L)              # but the plan gets one
  out <- plan$outputs[[1]]
  expect_true(identical(out$type, "wide") ||
                identical(out$type, "person_level"))
})

test_that("an empty recipe (no variables, no outputs) stays empty", {
  plan <- recipe_to_plan(omop_recipe())
  expect_length(plan$outputs, 0L)
})

# ---------------------------------------------------------------------------
# Footgun (d): ds.omop.cohort.create auto-assigns a non-colliding cohort id.
# ---------------------------------------------------------------------------

test_that("ds.omop.cohort.create gives un-id'd cohorts distinct, non-zero ids", {
  assign("omop", list(conns = list(s = "FAKE"), res_symbol = "dsO.x"),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = "omop", envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, ...) invisible(TRUE),
    .package = "DSI")

  spec <- list(type = "condition", concept_set = c(201820L))
  h1 <- ds.omop.cohort.create(spec = spec)
  h2 <- ds.omop.cohort.create(spec = spec)
  expect_match(unclass(h1)[1], "^dsomop_cohort_[0-9]+$")
  expect_false(identical(unclass(h1)[1], "dsomop_cohort_0"))
  expect_false(identical(unclass(h1)[1], unclass(h2)[1]))
  # A supplied id is honoured verbatim.
  h3 <- ds.omop.cohort.create(spec = spec, cohort_id = 7)
  expect_equal(unclass(h3)[1], "dsomop_cohort_7")
})
