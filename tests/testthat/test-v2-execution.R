# WS-8 (design-2.10.3 sections 4.4, 5, 7.3, 10.1): capability negotiation,
# v1 fallback, the publication status transport, the dynamic inventory and the
# strict/non-strict x error/skip matrix, against mocked servers.

.V2_ALL_OPS <- c(
  "concept_set", "event", "event.unit", "event.on_missing_table", "interval",
  "interval.ingredient", "interval.stockpile", "population.index", "population.first_eligible",
  "population.any_of", "population.criteria", "population.parent", "population.setop",
  "population.match", "population.coverage.union", "criteria.not", "criteria.age",
  "window.anchor.source", "variable.reducer", "variable.derived", "variable.raw",
  "variable.population_column", "output.wide", "output.long", "output.long.event_log",
  "output.survival", "output.survival.censor_source", "output.intervals",
  "output.temporal_covariates", "output.person_period", "output.person_period.partition",
  "output.sparse", "output.membership", "output.dictionary", "output.joined", "publish.roots",
  "event.episode_members", "event.cost", "reducer.supply_days",
  "reducer.covered_days.effective_supply", "status.accessor", "verb.mutate", "verb.filter_expr")

.caps <- function(ops = .V2_ALL_OPS, version = 2L, reducers = dsOMOPClient:::.V2_REDUCER_FNS) {
  list(recipe_language = list(version = version, semantic_version = "2.0.0",
                              operations = as.list(ops), reducers = as.list(reducers),
                              dialect = list(family = "sqlite")))
}

.with_servers <- function(caps, symbol = "omop") {
  conns <- stats::setNames(as.list(rep("FAKE", length(caps))), names(caps))
  assign(symbol, structure(list(conns = conns, res_symbol = "dsO_fake", capabilities = caps,
                                server_names = names(caps)),
                           class = "omop_session"),
         envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env))
      rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env))
  conns
}

.decode <- function(arg) {
  if (!is.character(arg) || length(arg) != 1L || !startsWith(arg, "B64:")) return(arg)
  b64 <- chartr("-_", "+/", sub("^B64:", "", arg))
  b64 <- paste0(b64, strrep("=", (4 - nchar(b64) %% 4) %% 4))
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)), simplifyVector = FALSE)
}

# A mocked federation: `statuses` gives each server's publication status (or an
# error string for an assign failure / a status read failure).
.mock_federation <- function(statuses, assign_fail = character(0), status_fail = character(0)) {
  log <- new.env()
  log$exec <- NULL; log$revoked <- character(0); log$removed <- character(0)
  log$symbols <- stats::setNames(rep(list(character(0)), length(statuses)), names(statuses))
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, success = NULL, error = NULL, ...) {
      executes <- identical(as.character(expr[[1L]]), "omopPlanExecuteDS")
      if (executes) log$exec <- expr
      for (s in names(conns)) {
        if (s %in% assign_fail) {
          if (is.function(error)) error(s, "server execution failed")
        } else {
          log$symbols[[s]] <- union(log$symbols[[s]], symbol)
          # a 2.10.1 server assigns the requested out symbols itself
          if (executes && is.null(statuses[[s]])) {
            log$symbols[[s]] <- union(log$symbols[[s]], unlist(.decode(expr[[4L]])))
          }
          if (is.function(success)) success(s)
        }
      }
      invisible(NULL)
    },
    datashield.aggregate = function(conns, expr, ...) {
      fn <- as.character(expr[[1L]])
      s <- names(conns)[[1L]]
      if (identical(fn, "omopPlanStatusDS")) {
        if (s %in% status_fail) stop("transport lost")
        return(stats::setNames(list(statuses[[s]]), s))
      }
      if (identical(fn, "omopStagedRevokeDS")) {
        log$revoked <- c(log$revoked, s)
        return(stats::setNames(list(TRUE), s))
      }
      stats::setNames(list(list()), s)
    },
    datashield.rm = function(conns, symbol, ...) {
      log$removed <- c(log$removed, paste0(names(conns), ":", symbol)); invisible(NULL)
    },
    datashield.symbols = function(conns, ...) log$symbols[names(conns)],
    datashield.errors = function(...) NULL,
    .package = "DSI", .env = parent.frame())
  testthat::local_mocked_bindings(
    .harmonizeConceptFactors = function(owned, conns) invisible(NULL),
    ds.omop.compare = function(symbol = "omop", conns = NULL, tables = NULL) {
      list(servers = names(conns), common_tables = tables, common_columns = list(),
           common_column_types = list(), column_errors = character(0),
           semantic_versions = stats::setNames(rep(list(list(vocabulary_version = "v5.0")),
                                                   length(conns)), names(conns)))
    },
    .package = "dsOMOPClient", .env = parent.frame())
  log
}

.ok_status <- function(published, rejected = list(), status = "ok") {
  list(status = status, published = published, rejected = rejected, unavailable = character(0))
}
.entry <- function(symbol, grain = "episode", n = 5L) list(symbol = symbol, grain = grain, n_columns = n)
.insufficient <- list(code = "insufficient_support",
                      message = dsOMOPClient:::.V2_INSUFFICIENT_MESSAGE)

.v2_recipe <- function() {
  mi <- omop_event("condition_occurrence", 4329847, name = "mi")
  omop_recipe(
    populations = omop_population("c1", index = omop_index(list(cohort_definition_id = 1), "all",
                                                          end = omop_end("event_end"))),
    variables = list(sex = omop_sex()),
    outputs = list(
      omop_output("baseline", rows = "c1", format = "wide"),
      omop_output("tte", rows = "c1", format = "survival", options = list(
        outcomes = list(mi = mi), censor = list("observation_end"),
        tars = list(itt = omop_window(1, NULL), short = omop_window(1, 30))))))
}
.legacy_recipe <- function() {
  omop_recipe(variables = omop_variable_sex(), outputs = omop_output("w", type = "wide"))
}

test_that("a 2.10.1-only recipe executes as its unchanged v1 plan on every server", {
  .with_servers(list(srv1 = list()))            # an old server: no recipe_language
  log <- .mock_federation(list(srv1 = NULL))
  recipe_execute(.legacy_recipe())
  sent <- .decode(log$exec[[3L]])
  expect_null(sent$version)                     # a v1 plan
  expect_identical(sent$outputs$w$type, "person_level")
  expect_false("commit_policy" %in% names(as.list(log$exec)))
})

test_that("a language-2 recipe is refused by an older server, naming the gap", {
  .with_servers(list(srv1 = list()))
  expect_error(recipe_execute(.v2_recipe()), "srv1 lacks recipe_language 2")
  .with_servers(list(srv1 = .caps(setdiff(.V2_ALL_OPS, "output.survival"))))
  expect_error(recipe_execute(.v2_recipe()), "srv1 lacks output.survival")
  .with_servers(list(srv1 = .caps(reducers = c("binary", "count"))))
  r <- omop_recipe(variables = list(x = omop_variable(event = omop_event("measurement", 1),
                                                      reducer = omop_reduce("mean"))),
                   outputs = omop_output("o", format = "wide"))
  expect_error(recipe_execute(r), "reducer mean")
})

test_that("parent inheritance is never dropped to fit an older server (B-8)", {
  .with_servers(list(srv1 = list()))
  r <- omop_recipe(populations = list(omop_population("a", filters = list(omop_filter_sex("F"))),
                                      omop_population("b", parent_id = "a",
                                                      filters = list(omop_filter_age(18, 40, year = 2020)))),
                   variables = omop_variable_sex(),
                   outputs = omop_output("w", type = "wide", population_id = "b"))
  expect_error(recipe_execute(r), "never dropped to fit an older server")
  expect_s3_class(recipe_to_plan(r, version = 1), "omop_plan")   # explicit v1 still compiles
})

test_that("v2 execution reads names from the publication inventory (multi-TAR, fresh names)", {
  .with_servers(list(srv1 = .caps()))
  log <- .mock_federation(list(srv1 = .ok_status(list(
    baseline = list(baseline = .entry("D_baseline_2")),
    tte = list(itt = .entry("D_tte.itt"), short = .entry("D_tte.short"))))))
  out <- recipe_execute(.v2_recipe())
  expect_identical(unclass(out)[c("baseline", "tte.itt", "tte.short")],
                   c(baseline = "D_baseline_2", tte.itt = "D_tte.itt", tte.short = "D_tte.short"))
  args <- as.list(log$exec)
  expect_identical(args$commit_policy, "server")
  sent <- .decode(args[[3L]])
  expect_identical(sent$version, 2L)
  expect_identical(sent$outputs$tte$options$tars$short$end, 30L)
  expect_true(any(grepl("srv1:dsOexec", log$removed)))   # status symbol deleted after reading
  expect_length(log$revoked, 0L)
  sess <- get("omop", envir = dsOMOPClient:::.dsomop_client_env)
  expect_true("D_tte.short" %in% sess$outputs)
})

test_that("strict refusal + on_small_site = 'error' rolls the federation back", {
  .with_servers(list(srv1 = .caps(), srv2 = .caps()))
  log <- .mock_federation(list(
    srv1 = .ok_status(list(baseline = list(baseline = .entry("D_baseline")),
                           tte = list(itt = .entry("D_tte.itt"), short = .entry("D_tte.short")))),
    srv2 = .ok_status(list(), rejected = list(tte = .insufficient), status = "refused")))
  expect_error(recipe_execute(.v2_recipe()), "insufficient individuals.*srv2")
  expect_setequal(log$revoked, c("srv1", "srv2"))
})

test_that("on_small_site = 'skip' keeps the other sites and returns NULL for the skipped one", {
  .with_servers(list(srv1 = .caps(), srv2 = .caps()))
  log <- .mock_federation(list(
    srv1 = .ok_status(list(baseline = list(baseline = .entry("D_baseline")),
                           tte = list(itt = .entry("D_tte.itt"), short = .entry("D_tte.short")))),
    srv2 = .ok_status(list(), rejected = list(tte = .insufficient), status = "refused")))
  expect_message(out <- recipe_execute(.v2_recipe(), on_small_site = "skip"), "Skipped.*srv2")
  expect_identical(as.list(log$exec)$commit_policy, "all_or_none")
  expect_null(attr(out, "inventory")$srv2)
  expect_identical(attr(out, "skipped"), "srv2")
  expect_identical(unclass(out)[["baseline"]], "D_baseline")
  expect_length(log$revoked, 0L)
})

test_that("non-strict partial publication warns naming the rejected family by code", {
  .with_servers(list(srv1 = .caps()))
  log <- .mock_federation(list(srv1 = .ok_status(
    list(baseline = list(baseline = .entry("D_baseline"))),
    rejected = list(tte = .insufficient), status = "partial")))
  expect_warning(out <- recipe_execute(.v2_recipe()),
                 "rejected families: srv1: tte \\(insufficient_support\\)")
  expect_identical(unclass(out)[["baseline"]], "D_baseline")
  expect_length(log$revoked, 0L)
})

test_that("only insufficient_support is skippable; other codes roll back under both settings", {
  for (mode in c("error", "skip")) {
    .with_servers(list(srv1 = .caps(), srv2 = .caps()))
    log <- .mock_federation(list(
      srv1 = .ok_status(list(baseline = list(baseline = .entry("D_baseline")))),
      srv2 = .ok_status(list(), status = "refused", rejected = list(tte = list(
        code = "coverage_ambiguous", message = "Ambiguous coverage.")))))
    expect_error(recipe_execute(.v2_recipe(), on_small_site = mode),
                 "other than the minimum number of persons.*coverage_ambiguous", info = mode)
    expect_setequal(log$revoked, c("srv1", "srv2"))
  }
})

test_that("transport, execution and inventory failures roll back and error", {
  .with_servers(list(srv1 = .caps(), srv2 = .caps()))
  st <- .ok_status(list(baseline = list(baseline = .entry("D_baseline"))))
  log <- .mock_federation(list(srv1 = st, srv2 = st), status_fail = "srv2")
  expect_error(recipe_execute(.v2_recipe()), "could not be read")
  expect_setequal(log$revoked, c("srv1", "srv2"))

  .with_servers(list(srv1 = .caps(), srv2 = .caps()))
  log <- .mock_federation(list(srv1 = st, srv2 = st), assign_fail = "srv1")
  expect_error(recipe_execute(.v2_recipe(), on_small_site = "skip"), "rolled back.*srv1")
  expect_setequal(log$revoked, c("srv1", "srv2"))

  .with_servers(list(srv1 = .caps(), srv2 = .caps()))
  log <- .mock_federation(list(
    srv1 = .ok_status(list(baseline = list(baseline = .entry("D_baseline")))),
    srv2 = .ok_status(list(baseline = list(baseline = .entry("D_baseline_2"))))))
  expect_error(recipe_execute(.v2_recipe()), "different names")
  expect_setequal(log$revoked, c("srv1", "srv2"))
})

test_that("v1 plans also use the status transport on 2.10.3 servers", {
  .with_servers(list(srv1 = .caps()))
  log <- .mock_federation(list(srv1 = .ok_status(list(w = list(w = .entry("D_w", "person"))))))
  out <- recipe_execute(.legacy_recipe())
  expect_null(.decode(log$exec[[3L]])$version)
  expect_identical(as.list(log$exec)$commit_policy, "server")
  expect_identical(unclass(out)[["w"]], "D_w")
})

test_that("malformed status objects are refused", {
  expect_error(dsOMOPClient:::.plan_validate_status(list(status = "fine"), "s"), "malformed")
  expect_error(dsOMOPClient:::.plan_validate_status(
    .ok_status(list(), rejected = list(x = list(code = "counts_were_7"))), "s"), "unknown rejection")
  expect_error(dsOMOPClient:::.plan_validate_status(
    .ok_status(list(x = list(x = list(grain = "person")))), "s"), "malformed publication inventory")
})

test_that("on_small_site = 'skip' is refused when a server has no status transport", {
  .with_servers(list(srv1 = list()))
  .mock_federation(list(srv1 = NULL))
  expect_error(recipe_execute(.legacy_recipe(), on_small_site = "skip"),
               "needs every server to report its publication status")
})
