# WS-9 standalone verbs, client side (design-2.10.3 sections 2.11, 4.4, 4.6,
# 7.3): ds.omop.mutate() translates R expressions into the closed mutate-v1
# AST (validated against the vendored schema, never evaluated) and
# ds.omop.filter(expr =) sends a plan-v2 predicate; both require the
# servers' verb operations. Transport is mocked; the server evaluation is
# covered by dsOMOP's test-verbs-v2.R.

.vb_decode <- function(arg) {
  b64 <- chartr("-_", "+/", sub("^B64:", "", arg))
  b64 <- paste0(b64, strrep("=", (4 - nchar(b64) %% 4) %% 4))
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)), simplifyVector = FALSE)
}

.vb_req <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  jsonlite::fromJSON(as.character(jsonlite::toJSON(
    dsOMOPClient:::.v2_plain(dsOMOPClient:::.mutate_request(exprs, parent.frame())),
    auto_unbox = TRUE, null = "null", digits = NA)), simplifyVector = FALSE)
}

# A fake two-server session whose DSI layer records assignments and copies.
.vb_backend <- function(ops = c("verb.mutate", "verb.filter_expr"), fail = character(0),
                        env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$symbols <- list(s1 = c("D", "E"), s2 = c("D", "E"))
  state$values <- list(s1 = list(D = "orig"), s2 = list(D = "orig"))
  state$calls <- list()
  conns <- list(s1 = "s1", s2 = "s2")
  state$methods <- .current_published_methods(conns)
  state$capabilities <- lapply(conns, function(x) {
    list(recipe_language = list(version = 2L, operations = as.list(ops)))
  })
  testthat::local_mocked_bindings(
    .get_session = function(...) list(conns = conns, capabilities = state$capabilities,
                                      last_output = "D"),
    .dsomop_published_methods = function(conns, ...) state$methods[names(conns)],
    .record_session_owned_symbol = function(...) invisible(NULL),
    .package = "dsOMOPClient", .env = env)
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state$symbols[names(conns)],
    datashield.assign.expr = function(conns, symbol, expr, success = NULL, error = NULL, ...) {
      state$calls[[length(state$calls) + 1L]] <- list(sites = names(conns), symbol = symbol, expr = expr)
      for (site in names(conns)) {
        if (is.call(expr) && site %in% fail) {
          if (is.function(error)) error(site, "Error: omopMutateDS: refused")
          next
        }
        value <- if (is.symbol(expr)) state$values[[site]][[as.character(expr)]] else "mutated"
        state$values[[site]][[symbol]] <- value
        state$symbols[[site]] <- union(state$symbols[[site]], symbol)
        if (is.function(success)) success(site)
      }
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      for (site in names(conns)) {
        state$symbols[[site]] <- setdiff(state$symbols[[site]], symbol)
        state$values[[site]][[symbol]] <- NULL
      }
      invisible(NULL)
    },
    .package = "DSI", .env = env)
  state
}

test_that("R expressions translate into the closed mutation AST", {
  req <- .vb_req(pdc = covered / observed, Pdc2 = -pdc, n = 5L, flag = ifelse(pdc >= 0.8, 1L, 0L),
                 grp = case_when(age < 18 ~ "child", is.na(age) ~ NA_character_, TRUE ~ "adult"),
                 band = cut(age, c(0, 18, 65), labels = c("young", "adult")),
                 band_left = cut(age, breaks = c(0, 18, 65), right = FALSE),
                 w = coalesce(weight, 0), adult = age >= 18 & !is.na(sex),
                 days = stop - as.Date("2020-01-01"), member = !(concept_id %in% c(1, 2)))
  a <- lapply(req$assign, function(x) x$expr)
  names(a) <- vapply(req$assign, function(x) x$name, "")
  expect_identical(req$version, 1L)
  expect_identical(a$pdc, list(op = "/", args = list(list(col = "covered"), list(col = "observed"))))
  expect_identical(a$Pdc2, list(op = "*", args = list(list(col = "pdc"), list(lit = -1L, as = "integer"))))
  expect_identical(a$n, list(lit = 5L, as = "integer"))
  expect_identical(a$flag, list(ifelse = list(test = list(var = "pdc", op = ">=", value = 0.8),
                                              yes = list(lit = 1L, as = "integer"),
                                              no = list(lit = 0L, as = "integer"))))
  expect_identical(a$grp$case_when$cases[[2L]],
                   list(when = list(var = "age", op = "is_null"), then = list(lit = NULL, as = "text")))
  expect_identical(a$grp$case_when[["else"]], list(lit = "adult"))
  expect_identical(a$band$cut, list(arg = list(col = "age"), breaks = list(0L, 18L, 65L), right = TRUE,
                                    labels = list("young", "adult")))
  expect_false(a$band_left$cut$right)
  expect_identical(a$w, list(coalesce = list(list(col = "weight"), list(lit = 0L))))
  expect_identical(a$adult, list(pred = list(and = list(list(var = "age", op = ">=", value = 18L),
                                                        list(var = "sex", op = "not_null")))))
  expect_identical(a$days$args[[2L]], list(lit = "2020-01-01", as = "date"))
  expect_identical(a$member, list(pred = list(var = "concept_id", op = "not_in", value = list(1L, 2L))))
  # the request validates against the vendored mutate-v1.json
  schema <- dsOMOPClient:::.jsonSchemaPackaged("mutate-v1", package = "dsOMOPClient")
  expect_true(dsOMOPClient:::.jsonSchemaValidate(schema, req)$valid)
  # breaks and labels may name vectors of the caller's session
  br <- c(0, 50, 100)
  expect_identical(.vb_req(b = cut(age, br))$assign[[1L]]$expr$cut$breaks, list(0L, 50L, 100L))
})

test_that("anything outside the grammar is refused on the client, nothing is evaluated", {
  hit <- FALSE
  side_effect <- function() { hit <<- TRUE; 1 }
  expect_error(.vb_req(x = side_effect()), "'side_effect' is not part of the mutation grammar")
  expect_error(.vb_req(x = system("id")), "not part of the mutation grammar")
  expect_error(.vb_req(x = log(age)), "not part of the mutation grammar")
  expect_error(.vb_req(x = a > b), "one column with one literal")
  expect_error(.vb_req(x = (a + 1) > 2), "one column with one literal")
  expect_error(.vb_req(x = ifelse(age, 1, 0)), "must be a condition")
  expect_error(.vb_req(x = cut(age, 3)), "at least two public breaks")
  expect_error(.vb_req(x = cut(age, c(0, 1), include.lowest = TRUE)), "does not take include.lowest")
  expect_error(.vb_req(x = age && sex), "use & and |", fixed = TRUE)
  expect_error(.vb_req(age * 2), "name every expression")
  expect_error(.vb_req(`1x` = age), "is not a column name")
  expect_error(.vb_req(x = `bad name`), "is not a column name")
  expect_error(.vb_req(x = Inf), "finite")
  expect_error(.vb_req(x = case_when(age > 1)), "condition ~ value")
  expect_error(dsOMOPClient:::.mutate_request(list(), environment()), "at least one")
  expect_false(hit)
})

test_that("ds.omop.mutate() sends one encoded request to a fresh symbol on every server", {
  state <- .vb_backend()
  out <- ds.omop.mutate("D", pdc = covered / observed, newobj = "D2")
  expect_identical(out, "D2")
  call <- state$calls[[1L]]
  expect_identical(call$symbol, "D2")
  expect_identical(as.character(call$expr[[1L]]), "omopMutateDS")
  expect_identical(call$expr[[2L]], as.name("D"))
  req <- .vb_decode(call$expr[[3L]])
  expect_identical(req$assign[[1L]]$name, "pdc")
  expect_true(all(vapply(state$symbols, function(s) "D2" %in% s, logical(1))))
  # default input: the session's last output; default newobj: a fresh name
  fresh <- ds.omop.mutate(y = 1L)
  expect_match(fresh, "^omop\\.mutate\\.")
  expect_error(ds.omop.mutate("D", y = 1L, newobj = "E"), "already exists")
})

test_that("ds.omop.mutate() needs verb.mutate on every server", {
  .vb_backend(ops = "verb.filter_expr")
  expect_error(ds.omop.mutate("D", y = 1L), "needs the 'verb.mutate' operation, which s1, s2 do not")
})

test_that("an in-place mutation replaces the target everywhere or nowhere", {
  state <- .vb_backend()
  expect_identical(ds.omop.mutate("D", y = 1L, newobj = "D"), "D")
  expect_identical(vapply(state$values, function(v) v$D, ""), c(s1 = "mutated", s2 = "mutated"))
  expect_false(any(grepl("^omop\\.rollback\\.", unlist(state$symbols))))
  # a refusal on one server restores the other from the rollback copy
  state <- .vb_backend(fail = "s2")
  expect_error(ds.omop.mutate("D", y = 1L, newobj = "D"), "was left unchanged")
  expect_identical(vapply(state$values, function(v) v$D, ""), c(s1 = "orig", s2 = "orig"))
  expect_false(any(grepl("^omop\\.rollback\\.", unlist(state$symbols))))
  copies <- Filter(function(cl) is.symbol(cl$expr), state$calls)
  expect_true(length(copies) >= 2L)   # the copy, then the restore on s1
})

test_that("ds.omop.filter(expr) sends the predicate tree; the legacy form is unchanged", {
  state <- .vb_backend()
  out <- ds.omop.filter("D", expr = omop_where(age >= 18, !is.na(bmi)), newobj = "F")
  expect_identical(out, "F")
  call <- state$calls[[1L]]
  expect_identical(as.character(call$expr[[1L]]), "omopFilterDS")
  expect_identical(names(call$expr)[[3L]], "expr")
  # JSON object member order is immaterial; transport now uses canonical
  # encoding to preserve every numeric boundary exactly.
  expect_identical(dsOMOPClient:::.jcsCanonical(.vb_decode(call$expr$expr)),
                   dsOMOPClient:::.jcsCanonical(list(and = list(
                     list(var = "age", op = ">=", value = 18L),
                     list(var = "bmi", op = "not_null")))))
  ds.omop.filter("D", expr = omop_where(bmi > omop_param("thr", type = "number", default = 30)),
                 newobj = "G")
  expect_identical(dsOMOPClient:::.jcsCanonical(.vb_decode(state$calls[[2L]]$expr$expr)),
                   dsOMOPClient:::.jcsCanonical(list(var = "bmi", op = ">", value = 30L)))
  expect_error(ds.omop.filter("D", var = "a", op = "==", value = 1, expr = omop_where(a == 1)),
               "not both")
  expect_error(ds.omop.filter("D", expr = "age > 1"), "omop_where")
  expect_error(ds.omop.filter("D", expr = omop_where(bmi > omop_param("u", type = "number"))),
               "Unbound parameter 'u'")
  ds.omop.filter("D", var = "sex", op = "==", value = "F", newobj = "H")
  legacy <- state$calls[[length(state$calls)]]$expr
  expect_identical(length(legacy), 5L)
  expect_null(names(legacy))
  .vb_backend(ops = "verb.mutate")
  expect_error(ds.omop.filter("D", expr = omop_where(age >= 18)), "verb.filter_expr")
})

# These requests all need the current protocol, including before mutate's
# in-place rollback copy. The older inventory is pinned to the 2.10.2 tag.
.vb_requests <- function() list(
  mutate = function(...) ds.omop.mutate("D", y = 1L, newobj = "M", ...),
  in_place = function(...) ds.omop.mutate("D", y = 1L, newobj = "D", ...),
  filter = function(...) ds.omop.filter("D", expr = omop_where(age >= 18), newobj = "F", ...)
)

test_that("verbs refuse tagged old and mixed profiles before any assignment or copy", {
  for (old in list(c("s1", "s2"), "s1", "s2")) {
    for (request in .vb_requests()) {
      state <- .vb_backend()
      state$methods[old] <- rep(list(.methods_2102()), length(old))
      # Keep advertised operations to catch stale capabilities: the published
      # inventory, independently of the session's block, must be checked.
      err <- tryCatch(request(), error = conditionMessage)
      expect_type(err, "character")
      expect_match(err, "dsOMOP 2.10.4", fixed = TRUE)
      expect_match(err, "[Nn]othing was sent to any site")
      for (s in old) expect_match(err, paste0(s, " lacks omop"))
      expect_length(state$calls, 0L)
      expect_identical(state$symbols, list(s1 = c("D", "E"), s2 = c("D", "E")))
      expect_identical(state$values, list(s1 = list(D = "orig"), s2 = list(D = "orig")))
    }
  }
})

test_that("every verb checks protocol and its assign method on the final site", {
  requests <- .vb_requests()
  for (verb in names(requests)) {
    method <- if (verb == "filter") "omopFilterDS" else "omopMutateDS"
    for (missing in c("omopPlanStatusDS", "omopStagedRevokeDS", method)) {
      state <- .vb_backend()
      state$methods$s2$assign <- setdiff(state$methods$s2$assign, missing)
      state$methods$s2$aggregate <- setdiff(state$methods$s2$aggregate, missing)
      # Publishing the method under the wrong type must not satisfy preflight.
      if (missing == method) state$methods$s2$aggregate <- c(state$methods$s2$aggregate, method)
      else state$methods$s2$assign <- c(state$methods$s2$assign, missing)
      expect_error(requests[[verb]](), paste0("s2 lacks ", missing))
      expect_length(state$calls, 0L)
    }
    state <- .vb_backend()
    state$methods$s2$error <- "profile unavailable"
    expect_error(requests[[verb]](), "Cannot check.*s2.*profile unavailable.*nothing was sent")
    expect_length(state$calls, 0L)
  }
})

test_that("verb preflight checks only participating sites and the methods it needs", {
  for (verb in names(.vb_requests())) {
    state <- .vb_backend()
    method <- if (verb == "filter") "omopFilterDS" else "omopMutateDS"
    # Plan compilers and factor methods are unnecessary for a standalone verb.
    state$methods$s1$assign <- method
    state$methods$s1$aggregate <- c("omopPlanStatusDS", "omopStagedRevokeDS")
    state$methods$s2 <- .methods_2102()
    expect_silent(.vb_requests()[[verb]](conns = list(s1 = "s1")))
    expect_true(length(state$calls) > 0L)
    expect_true(all(vapply(state$calls, function(x) identical(x$sites, "s1"), logical(1))))
    expect_identical(state$values$s2, list(D = "orig"))
  }
})

test_that("a final site's missing verb capability refuses even the rollback copy", {
  requests <- .vb_requests()
  for (verb in names(requests)) {
    op <- if (verb == "filter") "verb.filter_expr" else "verb.mutate"
    for (missing_block in c(FALSE, TRUE)) {
      state <- .vb_backend()
      if (missing_block) state$capabilities$s2 <- list()
      else state$capabilities$s2$recipe_language$operations <- list()
      expect_error(requests[[verb]](), paste0("needs the '", op, "' operation, which s2 does not"),
                   fixed = TRUE)
      expect_length(state$calls, 0L)
      expect_identical(state$values, list(s1 = list(D = "orig"), s2 = list(D = "orig")))
    }
  }
})

test_that("the legacy filter form remains usable with an old published profile", {
  state <- .vb_backend(ops = character(0))
  state$methods <- lapply(state$methods, function(x) .methods_2102())
  expect_identical(ds.omop.filter("D", var = "sex", op = "==", value = "F", newobj = "F"), "F")
  expect_length(state$calls, 1L)
  expect_identical(length(state$calls[[1L]]$expr), 5L)
})
