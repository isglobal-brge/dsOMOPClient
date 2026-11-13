# WS-8 language-2 execution fixtures, layered on WS-1's publication backend
# (helper-publication.R): recipe-language capabilities on the mocked session,
# a schema comparison for multi-server contracts, and a record of the plan
# execution call. No rows or person counts cross this API.

.V2_ALL_OPS <- c(
  "output.concept_labels", "concept_set", "event", "event.unit", "event.on_missing_table", "interval",
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

# Call after .publication_backend(): the mocked session gains the servers'
# capability blocks; the schema comparison of a multi-server contract and the
# factor harmonization (covered by WS-1's tests) are stubbed.
.with_caps <- function(caps, env = parent.frame()) {
  conns <- stats::setNames(as.list(names(caps)), names(caps))
  testthat::local_mocked_bindings(
    .get_session = function(...) list(conns = conns, res_symbol = "resource",
                                      capabilities = caps),
    ds.omop.compare = function(symbol = "omop", conns = NULL, tables = NULL) {
      list(servers = names(conns), common_tables = tables, common_columns = list(),
           common_column_types = list(), column_errors = character(0),
           semantic_versions = stats::setNames(rep(list(list(vocabulary_version = "v5.0")),
                                                   length(conns)), names(conns)))
    },
    .harmonizePublicationFactors = function(...) invisible(NULL),
    .package = "dsOMOPClient", .env = env)
  invisible(conns)
}

# Call after .publication_backend(): records the plan execution call and lets
# the backend handle it.
.capture_exec <- function(env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$exec <- NULL
  prev <- DSI::datashield.assign.expr
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, ...) {
      if (grepl("PlanExecuteDS$", as.character(expr[[1L]]))) log$exec <- expr
      prev(conns, symbol, expr, ...)
    }, .package = "DSI", .env = env)
  log
}

.decode <- function(arg) {
  if (!is.character(arg) || length(arg) != 1L || !startsWith(arg, "B64:")) return(arg)
  b64 <- chartr("-_", "+/", sub("^B64:", "", arg))
  b64 <- paste0(b64, strrep("=", (4 - nchar(b64) %% 4) %% 4))
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)), simplifyVector = FALSE)
}
