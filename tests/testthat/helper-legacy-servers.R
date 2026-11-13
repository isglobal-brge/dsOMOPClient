# WS-8 (review B1): a federation of dsOMOP 2.10.2 and current sites, with an
# assignment counter and a call log on every site. A 2.10.2 site publishes the
# real 2.10.2 method inventory (fixtures/dsomop-2.10.2/DESCRIPTION, tag 2.10.2,
# server commit 30b7905) and enforces its argument contract
# (fixtures/dsomop-2.10.2/contract.R): every argument its execution methods do
# not name goes to its own .omopAnalysisScopeFromDots(). No rows or person
# counts cross this API.

.fx_2102 <- function(...) testthat::test_path("fixtures", "dsomop-2.10.2", ...)

# The method inventory of dsOMOP 2.10.2, from its DESCRIPTION.
.methods_2102 <- function() {
  d <- read.dcf(.fx_2102("DESCRIPTION"))
  field <- function(f) {
    x <- trimws(strsplit(gsub("[[:space:]]+", " ", d[1L, f]), ",")[[1L]])
    x[nzchar(x)]
  }
  list(version = unname(d[1L, "Version"]), aggregate = field("AggregateMethods"),
       assign = field("AssignMethods"))
}

# The 2.10.2 argument contract: its scope validator, verbatim, and the
# signatures of its two execution methods.
.contract_2102 <- function() {
  env <- new.env(parent = baseenv())
  env$.omopDisclosureSettings <- function() list(max_analysis_scope_tables = 10L)
  env$.omopAnalysisScopeSourceCount <- function(...) invisible(0L)
  env$.omopAnalysisNormalizePublicScope <- identity
  sys.source(.fx_2102("contract.R"), envir = env)
  env
}

# Why a 2.10.2 server refuses an execution call, or NULL when it accepts it.
.check_2102_call <- function(contract, expr) {
  fn <- as.character(expr[[1L]])
  formal_fn <- contract[[paste0(".", fn, "_2.10.2")]]
  call <- expr
  call[[1L]] <- formal_fn
  matched <- as.list(match.call(formal_fn, call))[-1L]
  dots <- matched[setdiff(names(matched), setdiff(names(formals(formal_fn)), "..."))]
  tryCatch({
    contract$.omopAnalysisScopeFromDots(NULL, dots)
    NULL
  }, error = conditionMessage)
}

.decode_ds <- function(arg) {
  if (!is.character(arg) || length(arg) != 1L || !startsWith(arg, "B64:")) return(arg)
  b64 <- chartr("-_", "+/", sub("^B64:", "", arg))
  b64 <- paste0(b64, strrep("=", (4 - nchar(b64) %% 4) %% 4))
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)), simplifyVector = FALSE)
}

# Every site publishes the current method inventory (the publication
# protocol): the default for tests that mock the transport themselves.
.current_published_methods <- function(conns, ...) {
  current <- dsOMOPClient:::.DSOMOP_SERVER_METHODS
  stats::setNames(lapply(names(conns), function(s) {
    list(version = "2.10.3", aggregate = c(current$aggregate, current$optional),
         assign = current$assign)
  }), names(conns))
}
.mock_current_methods <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(.dsomop_published_methods = .current_published_methods,
                                  .package = "dsOMOPClient", .env = env)
}

# A federation with dsOMOP 2.10.2 sites `old` and current sites `new`.
# `unpublished` lists methods a current site's profile does not publish.
.legacy_federation <- function(old = character(0), new = character(0),
                               unpublished = character(0), ops = .V2_ALL_OPS,
                               env = parent.frame()) {
  sites <- c(old, new)
  m2102 <- .methods_2102()
  contract <- .contract_2102()
  current <- dsOMOPClient:::.DSOMOP_SERVER_METHODS
  state <- new.env(parent = emptyenv())
  state$assigns <- stats::setNames(rep(0L, length(sites)), sites)
  state$calls <- list()
  state$sent <- list()
  state$status <- list()
  state$symbols <- stats::setNames(rep(list(character(0)), length(sites)), sites)
  conns <- stats::setNames(as.list(sites), sites)
  caps <- stats::setNames(lapply(sites, function(s) if (s %in% old) list() else .caps(ops)), sites)
  method_rows <- function(s, type) {
    nm <- if (s %in% old) m2102[[type]] else {
      setdiff(c(current[[type]], if (identical(type, "aggregate")) current$optional), unpublished)
    }
    data.frame(server = s, name = nm, type = type, class = "function",
               value = paste0("dsOMOP::", nm), package = "dsOMOP",
               version = if (s %in% old) m2102$version else "2.10.3",
               stringsAsFactors = FALSE)
  }
  log <- function(...) state$calls[[length(state$calls) + 1L]] <- list(...)
  testthat::local_mocked_bindings(
    .get_session = function(...) list(conns = conns, res_symbol = "resource",
                                      capabilities = caps, server_names = sites),
    .prepare_plan_for_federation = function(plan, ...) plan,
    .session_harmonization_for_connections = function(...) NULL,
    .record_session_outputs = function(...) invisible(NULL),
    ds.omop.compare = function(symbol = "omop", conns = NULL, tables = NULL) {
      list(servers = names(conns), common_tables = tables, common_columns = list(),
           common_column_types = list(), column_errors = character(0),
           semantic_versions = stats::setNames(rep(list(list(vocabulary_version = "v5.0")),
                                                   length(conns)), names(conns)))
    },
    .package = "dsOMOPClient", .env = env)
  testthat::local_mocked_bindings(
    datashield.methods = function(conns, type = "aggregate", ...) {
      do.call(rbind, lapply(names(conns), method_rows, type = type))
    },
    datashield.symbols = function(conns, ...) state$symbols[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL, error = NULL, ...) {
      fn <- as.character(expr[[1L]])
      executes <- fn %in% c("omopPlanExecuteDS", "omopDpPlanExecuteDS")
      for (s in names(conns)) {
        log(site = s, type = "assign", method = fn,
            commit_policy = as.list(expr)$commit_policy)
        if (executes) {
          state$assigns[[s]] <- state$assigns[[s]] + 1L
          state$sent[[s]] <- expr
        }
        published <- if (s %in% old) m2102$assign else setdiff(current$assign, unpublished)
        if (!fn %in% published) {
          if (is.function(error)) error(s, paste0("No such DataSHIELD 'ASSIGN' method with name: ", fn))
          next
        }
        if (executes && s %in% old) {
          why <- .check_2102_call(contract, expr)
          if (!is.null(why)) {
            if (is.function(error)) error(s, why)
            next
          }
        }
        if (executes) {
          out <- .decode_ds(expr[[4L]])
          state$symbols[[s]] <- union(state$symbols[[s]], c(symbol, as.character(unlist(out))))
          state$status[[s]] <- list(
            status = "ok",
            published = lapply(out, function(sym) list(data = list(
              symbol = as.character(sym), grain = "person", n_columns = 2L))),
            rejected = list(), unavailable = character(0))
        } else {
          state$symbols[[s]] <- union(state$symbols[[s]], symbol)
        }
        if (is.function(success)) success(s)
      }
      invisible(NULL)
    },
    datashield.aggregate = function(conns, expr, success = NULL, error = NULL, ...) {
      fn <- as.character(expr[[1L]])
      res <- list()
      for (s in names(conns)) {
        log(site = s, type = "aggregate", method = fn)
        published <- if (s %in% old) m2102$aggregate else {
          setdiff(c(current$aggregate, current$optional), unpublished)
        }
        if (!fn %in% published) {
          msg <- paste0("No such DataSHIELD 'AGGREGATE' method with name: ", fn)
          if (is.function(error)) { error(s, msg); next }
          stop(msg)
        }
        value <- switch(fn,
          omopPlanStatusDS = state$status[[s]],
          omopStagedRevokeDS = TRUE,
          omopFactorLevelsDS = list(levels = list(), unsafe = character(0),
                                    nfilter_levels_max = 40L),
          stop("Unexpected aggregate: ", fn))
        res[[s]] <- value
        if (is.function(success)) success(s, value)
      }
      res
    },
    datashield.rm = function(conns, symbol, ...) {
      for (s in names(conns)) state$symbols[[s]] <- setdiff(state$symbols[[s]], symbol)
      invisible(NULL)
    },
    datashield.errors = function(...) NULL,
    .package = "DSI", .env = env)
  state
}

# The aggregate methods a federation was asked for.
.called <- function(state, type = "aggregate") {
  unique(vapply(Filter(function(x) identical(x$type, type), state$calls), `[[`, "", "method"))
}
