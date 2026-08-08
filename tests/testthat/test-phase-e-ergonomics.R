# ==============================================================================
# Tests: Phase E ergonomics (UNCOMMITTED)
#
#   (a) ds.omop.plan.execute `out=` overload semantics (.resolve_plan_out)
#   (b) session-symbol persistence (.record_session_outputs /
#       .resolve_target_symbol) and the manipulate-verb default target
#   (c) plan save / load round-trip (JSON + YAML), lossless w.r.t. transport
#   (d) ds.omop.disclosure.settings (read-only threshold inspection)
#
# Pure construction / serialization is unit-tested directly; the two places
# that need a "live" execute use a mocked DSI backend (datashield.assign.expr /
# datashield.rm / datashield.symbols captured), exactly like test-manipulate.R.
# ==============================================================================

# --- shared mock helpers ------------------------------------------------------

# Decode a B64: transport token exactly as the server's .ds_arg does.
.ergo_b64_decode <- function(x) {
  if (!is.character(x) || length(x) != 1L || !startsWith(x, "B64:")) return(x)
  b64 <- substring(x, 5)
  b64 <- gsub("-", "+", b64)
  b64 <- gsub("_", "/", b64)
  pad <- (4 - nchar(b64) %% 4) %% 4
  if (pad > 0) b64 <- paste0(b64, strrep("=", pad))
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)), simplifyVector = FALSE)
}

# Register a fake session (with res_symbol + conns) so .get_session resolves.
.ergo_with_session <- function(symbol = "omop",
                               extra = list()) {
  base <- list(res_symbol = "dsO_res", conns = list(srv = "FAKE"))
  assign(symbol, modifyList(base, extra),
         envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env)) {
      rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
    }
  )
}

# Read back the (possibly mutated) session object from the registry.
.ergo_session <- function(symbol = "omop") {
  get(symbol, envir = dsOMOPClient:::.dsomop_client_env)
}

.ergo_bind_plan <- function(plan) {
  plan$harmonization <- list(
    version = 2L, mode = "intersection",
    manifest = list(tables = list()), schema = list(),
    plan_signature = ""
  )
  plan$harmonization$plan_signature <-
    dsOMOPClient:::.plan_harmonization_signature(plan)
  plan
}

# Run an execute/manipulate call with DSI mocked; capture every assign.expr.
.ergo_capture <- function(fn) {
  calls <- list()
  symbols <- c("D_other", "D_base", "D_y")
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      calls[[length(calls) + 1L]] <<- list(symbol = symbol, expr = expr)
      symbols <<- union(symbols, symbol)
      if (is.call(expr) && identical(as.character(expr[[1L]]),
                                     "omopPlanExecuteDS")) {
        mapping <- .ergo_b64_decode(expr[[4L]])
        symbols <<- union(symbols, as.character(unlist(mapping,
                                                        use.names = FALSE)))
      }
      if (is.function(success)) success("srv")
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      symbols <<- setdiff(symbols, symbol)
      invisible(NULL)
    },
    datashield.symbols = function(conns, ...) list(srv = symbols),
    .package = "DSI"
  )
  ret <- fn()
  list(calls = calls, ret = ret)
}

# A small two-output plan; helper keeps each test focused.
.ergo_plan_two <- function() {
  p <- ds.omop.plan()
  p <- ds.omop.plan.cohort(p, cohort_definition_id = 1)
  p <- ds.omop.plan.baseline(p, columns = "gender_concept_id",
                             derived = character(0), name = "demo")
  p <- ds.omop.plan.events(p, name = "conds", table = "condition_occurrence",
                           concept_set = c(201826L, 443238L))
  p
}

# ------------------------------------------------------------------------------
# (a) out= overload — .resolve_plan_out branches
# ------------------------------------------------------------------------------

test_that("out=NULL auto-derives D_<name> for a single-output plan", {
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  expect_equal(dsOMOPClient:::.resolve_plan_out(p, NULL),
               c(baseline = "D_baseline"))
})

test_that("out=NULL auto-derives a symbol for EVERY output of a multi-output plan", {
  p <- .ergo_plan_two()
  expect_equal(dsOMOPClient:::.resolve_plan_out(p, NULL),
               c(demo = "D_demo", conds = "D_conds"))
})

test_that("out=NULL honours an output's result_symbol when set", {
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  p$outputs$baseline$result_symbol <- "MY_SYM"
  expect_equal(dsOMOPClient:::.resolve_plan_out(p, NULL),
               c(baseline = "MY_SYM"))
})

test_that("bare out='D' binds the single output of a single-output plan", {
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  expect_equal(dsOMOPClient:::.resolve_plan_out(p, "D"),
               c(baseline = "D"))
})

test_that("bare out='D' on a MULTI-output plan errors with the named-form hint", {
  p <- .ergo_plan_two()
  err <- expect_error(dsOMOPClient:::.resolve_plan_out(p, "D"))
  # lists the outputs and shows the named-form template
  expect_match(conditionMessage(err), "2 outputs")
  expect_match(conditionMessage(err), "demo")
  expect_match(conditionMessage(err), "conds")
  expect_match(conditionMessage(err), "Use the named form", fixed = TRUE)
})

test_that("named out=c(k='sym') is unchanged (regression) for one output", {
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  m <- c(baseline = "D_base")
  expect_identical(dsOMOPClient:::.resolve_plan_out(p, m), m)
})

test_that("named out maps each output of a multi-output plan (backward compatible)", {
  p <- .ergo_plan_two()
  m <- c(demo = "D_demo2", conds = "D_cond2")
  expect_identical(dsOMOPClient:::.resolve_plan_out(p, m), m)
})

test_that("named out rejects a name that is not a plan output", {
  p <- .ergo_plan_two()
  expect_error(
    dsOMOPClient:::.resolve_plan_out(p, c(demo = "D", nope = "X")),
    "not among plan outputs"
  )
})

test_that("out symbols are fully validated before execution", {
  p <- ds.omop.plan.baseline(ds.omop.plan(), name = "baseline")
  expect_error(dsOMOPClient:::.resolve_plan_out(p, NA_character_),
               "simple non-reserved")
  expect_error(dsOMOPClient:::.resolve_plan_out(p, "bad symbol"),
               "simple non-reserved")
  expect_error(dsOMOPClient:::.resolve_plan_out(
    p, "omop", reserved_symbols = "omop"
  ), "cannot target OMOP resources")
})

test_that("a plan with no outputs errors early", {
  expect_error(dsOMOPClient:::.resolve_plan_out(ds.omop.plan(), NULL),
               "no outputs")
  expect_error(dsOMOPClient:::.resolve_plan_out(ds.omop.plan(), "D"),
               "no outputs")
})

test_that("non-character / empty out is rejected", {
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  expect_error(dsOMOPClient:::.resolve_plan_out(p, 42), "must be NULL")
  expect_error(dsOMOPClient:::.resolve_plan_out(p, character(0)), "must be NULL")
})

# --- end-to-end: ds.omop.plan.execute drives the resolved mapping -------------

test_that("execute with out=NULL sends the auto-derived mapping and returns it", {
  .ergo_with_session()
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  p <- ds.omop.plan.options(p, factor_concepts = FALSE)  # skip harmonization

  res <- .ergo_capture(function() ds.omop.plan.execute(p))
  expect_equal(res$ret, c(baseline = "D_baseline"))
  # the executeDS assign carries the resolved out mapping as its 3rd DS arg.
  # A single-element named vector is under .ds_encode's multi-element threshold,
  # so it travels bare (still carrying its name) rather than B64-wrapped.
  exec <- res$calls[[1]]$expr
  expect_equal(as.character(exec[[1]]), "omopPlanExecuteDS")
  expect_equal(exec[[4]], c(baseline = "D_baseline"))
})

test_that("execute B64-encodes a multi-output mapping over the transport", {
  .ergo_with_session()
  p <- .ergo_plan_two()
  p <- ds.omop.plan.options(p, factor_concepts = FALSE)

  res <- .ergo_capture(function() ds.omop.plan.execute(p))
  expect_equal(res$ret, c(demo = "D_demo", conds = "D_conds"))
  exec <- res$calls[[1]]$expr
  expect_equal(.ergo_b64_decode(exec[[4]]),
               list(demo = "D_demo", conds = "D_conds"))
})

test_that("execute with bare out='D' binds the sole output", {
  .ergo_with_session()
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  p <- ds.omop.plan.options(p, factor_concepts = FALSE)

  res <- .ergo_capture(function() ds.omop.plan.execute(p, out = "D"))
  expect_equal(res$ret, c(baseline = "D"))
})

test_that("execute with bare out='D' on a multi-output plan errors before any assign", {
  .ergo_with_session()
  p <- .ergo_plan_two()
  p <- ds.omop.plan.options(p, factor_concepts = FALSE)

  res <- NULL
  expect_error(
    res <- .ergo_capture(function() ds.omop.plan.execute(p, out = "D")),
    "single-output"
  )
})

test_that("expected output ownership is exact for OHDSI composite formats", {
  p <- ds.omop.plan.events(
    ds.omop.plan(), "sparse", "condition_occurrence",
    concept_set = 201820L, representation = list(format = "sparse")
  )
  p <- ds.omop.plan.temporal_covariates(
    p, "condition_occurrence", 201820L, name = "temporal"
  )
  p <- ds.omop.plan.person_period(
    p, "condition_occurrence", 201820L, name = "panel"
  )
  p <- ds.omop.plan.survival(
    p,
    outcomes = list(mi = list(
      table = "condition_occurrence", concept_set = 201820L
    )),
    format = "recurrent_events", event_order = "all", name = "recurrent"
  )
  owned <- dsOMOPClient:::.plan_expected_output_symbols(
    p, c(sparse = "S", temporal = "T", panel = "P", recurrent = "R")
  )
  expect_identical(owned$sparse,
                   c("S.covariates", "S.covariateRef", "S.personRef"))
  expect_identical(owned$temporal, c(
    "T.temporalCovariates", "T.covariateRef", "T.timeRef", "T.personRef"
  ))
  expect_identical(owned$panel, c(
    "P.temporalCovariates", "P.covariateRef", "P.timeRef", "P.personRef",
    "P.personPeriods"
  ))
  expect_identical(owned$recurrent, c("R.events", "R.riskSets"))

  family <- dsOMOPClient:::.plan_output_symbol_families(c(sparse = "S"))
  expect_identical(family$sparse, c(
    "S", "S.covariates", "S.covariateRef", "S.personRef",
    "S.temporalCovariates", "S.timeRef", "S.personPeriods", "S.events",
    "S.riskSets", "S.msdata", "S.transitionRef"
  ))
  expect_error(
    dsOMOPClient:::.plan_output_symbol_families(
      c(first = "D", second = "D.covariates")
    ),
    "overlapping reserved component families"
  )
})

test_that("execute keeps unrelated objects and cleans a partial fresh output", {
  symbol <- "atomic_execute"
  conns <- list(a = "A", b = "B")
  assign(symbol, list(
    res_symbol = "dsO_res", conns = conns,
    outputs = "D.model", last_output = "D.model"
  ),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  state <- list(
    a = "D.model",
    b = "D.model"
  )
  removed <- list(a = character(0), b = character(0))
  testthat::local_mocked_bindings(
    .session_harmonization_for_connections = function(...) NULL,
    ds.omop.plan.harmonize = function(plan, ...) .ergo_bind_plan(plan),
    .package = "dsOMOPClient"
  )
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.rm = function(conns, target, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], target)
      removed[[server]] <<- c(removed[[server]], target)
      invisible(NULL)
    },
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      state$a <<- union(state$a, "D")
      success("a")
      error("b", "backend failed")
      invisible(NULL)
    },
    .package = "DSI"
  )
  p <- ds.omop.plan.events(
    ds.omop.plan(), "events", "condition_occurrence",
    concept_set = 201820L
  )
  p <- ds.omop.plan.options(p, factor_concepts = FALSE,
                            translate_concepts = FALSE)
  expect_error(
    ds.omop.plan.execute(p, out = c(events = "D"), symbol = symbol),
    "failed or was incomplete"
  )
  expect_false("D" %in% state$a)
  expect_false("D" %in% state$b)
  expect_true("D.model" %in% state$a)
  expect_true("D.model" %in% state$b)
  expect_true(all(vapply(removed, function(x) "D" %in% x, logical(1))))
  expect_identical(dsOMOPClient:::.get_session(symbol)$last_output, "D.model")
  expect_identical(dsOMOPClient:::.get_session(symbol)$outputs, "D.model")
})

test_that("execute refuses to overwrite any pre-existing output family", {
  symbol <- "fresh_execute"
  conns <- list(a = "A", b = "B")
  assign(symbol, list(res_symbol = "dsO_res", conns = conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  assigned <- FALSE
  testthat::local_mocked_bindings(
    .session_harmonization_for_connections = function(...) NULL,
    ds.omop.plan.harmonize = function(plan, ...) .ergo_bind_plan(plan),
    .package = "dsOMOPClient"
  )
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) {
      stats::setNames(rep(list(c("D", "other_session")), length(conns)),
                      names(conns))
    },
    datashield.assign.expr = function(...) {
      assigned <<- TRUE
      invisible(NULL)
    },
    .package = "DSI"
  )
  p <- ds.omop.plan.events(
    ds.omop.plan(), "events", "condition_occurrence",
    concept_set = 201820L
  )
  p <- ds.omop.plan.options(p, factor_concepts = FALSE,
                            translate_concepts = FALSE)

  expect_error(
    ds.omop.plan.execute(p, out = c(events = "D"), symbol = symbol),
    "must be fresh"
  )
  expect_false(assigned)
})

test_that("execute rejects a missing composite component after server success", {
  symbol <- "atomic_components"
  conns <- list(a = "A", b = "B")
  assign(symbol, list(res_symbol = "dsO_res", conns = conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  state <- list(a = character(0), b = character(0))
  expected <- c("D.covariates", "D.covariateRef", "D.personRef")
  testthat::local_mocked_bindings(
    .session_harmonization_for_connections = function(...) NULL,
    ds.omop.plan.harmonize = function(plan, ...) .ergo_bind_plan(plan),
    .package = "dsOMOPClient"
  )
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.rm = function(conns, target, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], target)
      invisible(NULL)
    },
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      state$a <<- expected
      state$b <<- setdiff(expected, "D.personRef")
      success("a"); success("b")
      invisible(NULL)
    },
    .package = "DSI"
  )
  p <- ds.omop.plan.events(
    ds.omop.plan(), "events", "condition_occurrence",
    concept_set = 201820L, representation = list(format = "sparse")
  )
  p <- ds.omop.plan.options(p, factor_concepts = FALSE,
                            translate_concepts = FALSE)
  expect_error(
    ds.omop.plan.execute(p, out = c(events = "D"), symbol = symbol),
    "did not materialize every requested output component"
  )
  expect_length(state$a, 0L)
  expect_length(state$b, 0L)
})

test_that("multi-server execute strictly harmonizes an unbound plan", {
  symbol <- "automatic_schema_harmonization"
  conns <- list(a = "A", b = "B")
  assign(symbol, list(res_symbol = "dsO_res", conns = conns),
         envir = dsOMOPClient:::.dsomop_client_env)
  on.exit(rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)
  state <- list(a = character(0), b = character(0))
  called <- FALSE
  testthat::local_mocked_bindings(
    ds.omop.plan.harmonize = function(plan, mode, strict, symbol, conns) {
      called <<- TRUE
      expect_identical(mode, "intersection")
      expect_true(strict)
      .ergo_bind_plan(plan)
    },
    .session_harmonization_for_connections = function(...) NULL,
    .package = "dsOMOPClient"
  )
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.rm = function(conns, target, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], target)
      invisible(NULL)
    },
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      state$a <<- union(state$a, "D")
      state$b <<- union(state$b, "D")
      success("a"); success("b")
      invisible(NULL)
    },
    .package = "DSI"
  )
  p <- ds.omop.plan.events(
    ds.omop.plan(), "events", "condition_occurrence",
    concept_set = 201820L
  )
  p <- ds.omop.plan.options(p, factor_concepts = FALSE,
                            translate_concepts = FALSE)
  expect_identical(
    ds.omop.plan.execute(p, out = c(events = "D"), symbol = symbol),
    c(events = "D")
  )
  expect_true(called)
})

test_that("failed-execution cleanup fails closed when removal is unprovable", {
  conns <- list(a = "A")
  testthat::local_mocked_bindings(
    datashield.rm = function(...) invisible(NULL),
    datashield.symbols = function(...) list(a = "D"),
    .package = "DSI"
  )
  expect_error(
    dsOMOPClient:::.plan_remove_output_symbols(
      conns, list(result = "D"), verify = TRUE
    ),
    "cleanup could not be proven"
  )
})

# ------------------------------------------------------------------------------
# (b) session-symbol persistence
# ------------------------------------------------------------------------------

test_that(".record_session_outputs records accumulated outputs + last_output", {
  .ergo_with_session()
  dsOMOPClient:::.record_session_outputs("omop", c(a = "D_a", b = "D_b"))
  s <- .ergo_session()
  expect_equal(s$outputs, c("D_a", "D_b"))
  expect_equal(s$last_output, "D_b")

  # a second execution accumulates (de-duped) and advances last_output
  dsOMOPClient:::.record_session_outputs("omop", c(b = "D_b", c = "D_c"))
  s <- .ergo_session()
  expect_equal(s$outputs, c("D_a", "D_b", "D_c"))
  expect_equal(s$last_output, "D_c")

  dsOMOPClient:::.record_session_outputs(
    "omop", list(sparse = c("S.covariates", "S.covariateRef", "S.personRef"))
  )
  s <- .ergo_session()
  expect_true(all(c("S.covariates", "S.covariateRef", "S.personRef") %in%
                    s$outputs))
  expect_equal(s$last_output, "S.covariates")
})

test_that(".record_session_outputs is a no-op when the session is absent", {
  # no session registered under this symbol
  if (exists("ghost", envir = dsOMOPClient:::.dsomop_client_env)) {
    rm(list = "ghost", envir = dsOMOPClient:::.dsomop_client_env)
  }
  expect_silent(dsOMOPClient:::.record_session_outputs("ghost", c(a = "D_a")))
  expect_false(exists("ghost", envir = dsOMOPClient:::.dsomop_client_env))
})

test_that(".resolve_target_symbol falls back to session$last_output", {
  s <- list(last_output = "D_last")
  expect_equal(dsOMOPClient:::.resolve_target_symbol(NULL, s, "x"), "D_last")
  # explicit value always wins over the session default
  expect_equal(dsOMOPClient:::.resolve_target_symbol("D_explicit", s, "x"),
               "D_explicit")
})

test_that(".resolve_target_symbol errors clearly when nothing is recorded", {
  s <- list()  # no last_output
  expect_error(dsOMOPClient:::.resolve_target_symbol(NULL, s, "x"),
               "no recorded output")
})

test_that("execute records last_output so a later manipulate verb defaults to it", {
  .ergo_with_session()
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  p <- ds.omop.plan.options(p, factor_concepts = FALSE)

  res <- .ergo_capture(function() {
    ds.omop.plan.execute(p)                       # records D_baseline
    # select WITHOUT x: must target the recorded last_output
    ds.omop.select(cols = c("gender_concept_id"), newobj = "slim")
  })

  expect_equal(.ergo_session()$last_output, "D_baseline")
  # the LAST captured assign is the select; its 1st DS arg is the target symbol
  sel <- res$calls[[length(res$calls)]]$expr
  expect_equal(as.character(sel[[1]]), "omopSelectDS")
  expect_equal(sel[[2]], as.name("D_baseline"))
})

test_that("an explicit x overrides the session default in a manipulate verb", {
  .ergo_with_session(extra = list(last_output = "D_baseline"))

  res <- .ergo_capture(function()
    ds.omop.select("D_other", cols = "age", newobj = "slim"))
  sel <- res$calls[[length(res$calls)]]$expr
  expect_equal(sel[[2]], as.name("D_other"))
})

test_that("filter / merge / bind_rows all honour the session default for x", {
  .ergo_with_session(extra = list(last_output = "D_base"))

  res <- .ergo_capture(function() {
    ds.omop.filter(var = "sex", op = "==", value = "F", newobj = "f")
    ds.omop.merge(y = "D_y", newobj = "m")
    ds.omop.bind_rows(y = "D_y", newobj = "b")
  })
  exprs <- lapply(res$calls, function(c) c$expr)
  # x is the first DS arg in each; all default to D_base
  expect_equal(exprs[[1]][[2]], as.name("D_base"))  # filter
  expect_equal(exprs[[2]][[2]], as.name("D_base"))  # merge
  expect_equal(exprs[[3]][[2]], as.name("D_base"))  # bind_rows
})

# ------------------------------------------------------------------------------
# (c) plan save / load
# ------------------------------------------------------------------------------

# A non-trivial plan: cohort, nested and/or custom filters, concept_set,
# time_window, temporal spec, multiple outputs, options.
.ergo_plan_rich <- function() {
  p <- ds.omop.plan()
  p <- ds.omop.plan.cohort(p, cohort_definition_id = 7)
  p <- ds.omop.plan.baseline(p,
    columns = c(sex = "gender_concept_id", "year_of_birth"),
    derived = c("age_at_index"), name = "demo")
  p <- ds.omop.plan.events(p,
    name = "labs", table = "measurement",
    concept_set = c(3004410L, 3013682L),
    time_window = list(start_date = "2018-01-01", end_date = "2020-12-31"),
    temporal = omop.temporal(index_window = list(start = -365L, end = 0L),
                             event_select = list(order = "first", n = 3L)),
    filters = list(op = "and", children = list(
      list(var = "unit_concept_id", op = "==", value = 8840L),
      list(op = "or", children = list(
        list(var = "value_as_number", op = ">", value = 5),
        list(var = "value_as_number", op = "<", value = 1)
      ))
    )),
    representation = list(format = "long"))
  p <- ds.omop.plan.options(p, translate_concepts = FALSE,
                            factor_concepts = TRUE)
  p
}

test_that("plan round-trips losslessly through JSON (class, deep-equal, transport)", {
  p <- .ergo_plan_rich()
  f <- withr::local_tempfile(fileext = ".json")

  expect_equal(ds.omop.plan.save(p, f), f)         # returns the path
  expect_true(file.exists(f))
  loaded <- ds.omop.plan.load(f)

  expect_s3_class(loaded, "omop_plan")
  # structural deep-equality of every field carried in the plain representation
  expect_equal(dsOMOPClient:::.plan_plain(loaded), dsOMOPClient:::.plan_plain(p))
  # would execute identically: the exact transport encoder is byte-identical
  expect_identical(dsOMOPClient:::.ds_encode(loaded),
                   dsOMOPClient:::.ds_encode(p))
})

test_that("plan round-trips losslessly through YAML (class, deep-equal, transport)", {
  p <- .ergo_plan_rich()
  f <- withr::local_tempfile(fileext = ".yaml")

  expect_equal(ds.omop.plan.save(p, f), f)
  loaded <- ds.omop.plan.load(f)

  expect_s3_class(loaded, "omop_plan")
  expect_equal(dsOMOPClient:::.plan_plain(loaded), dsOMOPClient:::.plan_plain(p))
  expect_identical(dsOMOPClient:::.ds_encode(loaded),
                   dsOMOPClient:::.ds_encode(p))
})

test_that("integer concept/offset fields survive the round-trip as integers", {
  p <- .ergo_plan_rich()
  f <- withr::local_tempfile(fileext = ".json")
  ds.omop.plan.save(p, f)
  loaded <- ds.omop.plan.load(f)
  expect_type(loaded$outputs$labs$filters$concept_set$ids, "integer")
  expect_identical(loaded$cohort$cohort_definition_id, 7L)
})

test_that(".yml extension is accepted and dispatches to YAML", {
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  f <- withr::local_tempfile(fileext = ".yml")
  ds.omop.plan.save(p, f)
  expect_s3_class(ds.omop.plan.load(f), "omop_plan")
})

test_that("an explicit format on save overrides the file extension", {
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  f <- withr::local_tempfile(fileext = ".dat")
  ds.omop.plan.save(p, f, format = "json")
  # .plan_file_format resolves the override, and the bytes parse as JSON.
  expect_equal(dsOMOPClient:::.plan_file_format(f, "json"), "json")
  parsed <- jsonlite::fromJSON(paste(readLines(f, warn = FALSE),
                                     collapse = "\n"),
                               simplifyVector = FALSE)
  expect_equal(parsed$version, "1.1")
})

test_that("plan persistence retains populations, scope, and schema binding", {
  p <- .ergo_plan_rich()
  p$populations <- list(base = list(
    id = "base", kind = "criteria",
    filter_tree = list(type = "sex", params = list(value = "F"))
  ))
  p$scope <- list(tables = c("eligible_a", "eligible_b"),
                  combine = "intersect")
  p$harmonization <- list(
    version = 1L, mode = "intersection", strict = TRUE,
    schema = list(
      servers = c("a", "b"), tables = "person",
      present = list(person = TRUE),
      columns = list(person = c("gender_concept_id", "person_id")),
      types = list(person = list(
        gender_concept_id = "numeric", person_id = "numeric"
      ))
    ),
    plan_signature = NULL
  )
  p$harmonization$plan_signature <-
    dsOMOPClient:::.plan_harmonization_signature(p)
  f <- withr::local_tempfile(fileext = ".json")
  ds.omop.plan.save(p, f)
  loaded <- ds.omop.plan.load(f)
  expect_identical(loaded$populations, p$populations)
  expect_identical(loaded$scope, p$scope)
  expect_identical(loaded$harmonization, p$harmonization)
  expect_identical(
    dsOMOPClient:::.plan_harmonization_signature(loaded),
    p$harmonization$plan_signature
  )
})

test_that("save/load reject bad inputs and unknown formats", {
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  expect_error(ds.omop.plan.save(list(), tempfile(fileext = ".json")),
               "omop_plan")
  expect_error(ds.omop.plan.save(p, tempfile(fileext = ".txt")),
               "json.*yaml|yaml.*json")
  # load checks existence first, so the file must exist for the format error.
  bad <- withr::local_tempfile(fileext = ".txt")
  writeLines("not a plan", bad)
  expect_error(ds.omop.plan.load(bad), "json.*yaml|yaml.*json")
  expect_error(ds.omop.plan.load(tempfile(fileext = ".json")), "not found")
})

test_that("the YAML path errors cleanly when the 'yaml' package is missing", {
  p <- ds.omop.plan()
  p <- ds.omop.plan.baseline(p, name = "baseline")
  f <- withr::local_tempfile(fileext = ".yaml")

  # Force the guard to believe 'yaml' is unavailable.
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "yaml")) FALSE else base::requireNamespace(package, ...)
    },
    .package = "base"
  )
  expect_error(ds.omop.plan.save(p, f), "yaml.*required")
})

# ------------------------------------------------------------------------------
# (d) ds.omop.disclosure.settings — read-only threshold inspection
# ------------------------------------------------------------------------------

test_that("ds.omop.disclosure.settings returns the active per-server thresholds", {
  .ergo_with_session()
  fake <- list(srv = list(nfilter_subset = 3, nfilter_tab = 3,
                          nfilter_levels_max = 40))
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      # the endpoint is the read-only aggregate, called with NO data symbol
      expect_equal(as.character(expr[[1]]), "omopDisclosureSettingsDS")
      expect_length(expr, 1L)
      fake[names(conns)]
    },
    .package = "DSI"
  )

  res <- ds.omop.disclosure.settings()
  expect_equal(res$srv$nfilter_subset, 3)
  expect_equal(res$srv$nfilter_levels_max, 40)
})

test_that("ds.omop.disclosure.settings offers NO argument to lower a threshold", {
  # Read-only by construction: the public signature exposes only symbol/conns,
  # never a setter for any nfilter_* floor.
  expect_setequal(names(formals(ds.omop.disclosure.settings)),
                  c("symbol", "conns"))
})
