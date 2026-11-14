.publication_plan <- function(roots = "data") {
  plan <- ds.omop.plan()
  # Transport-only fixtures do not implement categorical reconciliation.
  for (root in roots) plan <- ds.omop.plan.baseline(plan, name = root, derived = character())
  ds.omop.plan.options(plan, factor_concepts = FALSE, translate_concepts = FALSE)
}

test_that("status is retrieved before deletion and dynamic symbols are returned", {
  statuses <- list(a = .publication_status(c(data = "D.1")),
                   b = .publication_status(c(data = "D")))
  state <- .publication_backend(statuses, initial = list(a = "D", b = character()))
  result <- ds.omop.plan.execute(.publication_plan(), out = "D")
  expect_identical(result$a$data$data$symbol, "D.1")
  expect_identical(result$b$data$data$symbol, "D")
  expect_identical(state$policies, "server")
  expect_setequal(state$symbols$a, c("D", "D.1"))
  expect_identical(state$calls[[1L]]$method, "omopPlanStatusDS")
  expect_length(state$revoked, 0L)
})

test_that("ordinary successful publication retains its v1 mapping", {
  .publication_backend(list(a = .publication_status(), b = .publication_status()))
  expect_identical(ds.omop.plan.execute(.publication_plan(), out = "D"),
                   c(data = "D"))
})

test_that("status retrieval treats only mapped v1 outputs as publication roots", {
  plan <- .publication_plan(c("conditions", "other"))
  out <- c(conditions = "cond_df")
  statuses <- list(a = .publication_status(out))
  conns <- list(a = "A")
  expect_null(plan$publish)
  expect_named(plan$outputs, c("conditions", "other"))
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      expect_identical(expr, call("omopPlanStatusDS", "execution"))
      statuses[names(conns)]
    }, .package = "DSI"
  )
  expect_identical(dsOMOPClient:::.plan_read_status(
    conns, "execution", plan, out
  ), statuses)

  # A server publishing the unmapped output must still fail transport validation.
  statuses$a <- .publication_status(c(out, other = "other"))
  expect_error(dsOMOPClient:::.plan_read_status(
    conns, "execution", plan, out
  ), "Invalid plan publication status")
})

test_that("partial v1 out mappings retain only requested published symbols", {
  plan <- .publication_plan(c("conditions", "other"))
  out <- c(conditions = "cond_df")
  state <- .publication_backend(list(a = .publication_status(out)))
  expect_identical(ds.omop.plan.execute(plan, out = out), out)
  expect_identical(state$symbols$a, "cond_df")
  expect_identical(state$calls[[1L]]$method, "omopPlanStatusDS")
  expect_length(state$revoked, 0L)
})

test_that("strict/error refuses and rolls back every site", {
  statuses <- list(a = .publication_status(),
                   b = .publication_status(character(), "refused",
                                           list(data = .publication_rejection())))
  state <- .publication_backend(statuses)
  expect_error(ds.omop.plan.execute(.publication_plan(), out = "D"),
               "b:data \\[insufficient_support\\]")
  expect_setequal(state$revoked, c("a", "b"))
  expect_true(all(lengths(state$symbols) == 0L))
})

test_that("strict and non-strict skip request all-or-none before publication", {
  # Server mode is intentionally absent from the client protocol: both servers
  # return refused after applying all_or_none to their final-support rejection.
  for (server_mode in c("strict", "non-strict")) {
    statuses <- list(a = .publication_status(),
                     b = .publication_status(character(), "refused",
                                             list(data = .publication_rejection())))
    state <- .publication_backend(statuses)
    result <- ds.omop.plan.execute(.publication_plan(), out = "D",
                                   on_small_site = "skip")
    expect_identical(state$policies, "all_or_none", info = server_mode)
    expect_null(result$b)
    expect_identical(result$a$data$data$symbol, "D")
    expect_length(state$revoked, 0L)
  }
})

test_that("non-strict/error keeps complete families and warns with typed codes", {
  statuses <- list(a = .publication_status(c(data = "D", small = "S")),
                   b = .publication_status(c(data = "D"), "partial",
                                           list(small = .publication_rejection())))
  state <- .publication_backend(statuses)
  expect_warning(result <- ds.omop.plan.execute(
    .publication_plan(c("data", "small")), out = c(data = "D", small = "S")
  ), "b:small \\[insufficient_support\\]")
  expect_named(result$b, "data")
  expect_setequal(state$symbols$a, c("D", "S"))
  expect_identical(state$symbols$b, "D")
  expect_length(state$revoked, 0L)
})

test_that("only insufficient support is skippable", {
  for (code in c("coverage_ambiguous", "join_cardinality",
                 "match_candidates_exceeded", "dialect_unsupported",
                 "unavailable_table", "validation_error")) {
    statuses <- list(a = .publication_status(),
                     b = .publication_status(character(), "refused",
                                             list(data = .publication_rejection(code))))
    state <- .publication_backend(statuses)
    expect_error(ds.omop.plan.execute(.publication_plan(), out = "D",
                                      on_small_site = "skip"), code)
    expect_setequal(state$revoked, c("a", "b"))
  }
})

test_that("status retrieval failure revokes staged artifacts through the server", {
  state <- .publication_backend(list(a = .publication_status(),
                                     b = .publication_status()), retrieve_error = TRUE)
  expect_error(ds.omop.plan.execute(.publication_plan(), out = "D",
                                    output_mode = "staged", on_small_site = "skip"),
               "status transport failed")
  expect_setequal(state$revoked, c("a", "b"))
  expect_true(all(lengths(state$symbols) == 0L))
})

test_that("failed assignment rolls back successful sites without deleting user objects", {
  state <- .publication_backend(list(a = .publication_status(),
                                     b = .publication_status()),
                                initial = list(a = "D.model", b = "D.model"),
                                assign_error = "b")
  expect_error(ds.omop.plan.execute(.publication_plan(), out = "D"),
               "backend failed")
  expect_identical(state$symbols, list(a = "D.model", b = "D.model"))
})

test_that("missing dynamic components and malformed statuses fail closed", {
  state <- .publication_backend(list(a = .publication_status()), missing = "D")
  expect_error(ds.omop.plan.execute(.publication_plan(), out = "D"),
               "did not materialize every published output component")
  expect_identical(state$revoked, "a")
  malformed <- .publication_status()
  malformed$private_count <- 5L
  state <- .publication_backend(list(a = malformed))
  expect_error(ds.omop.plan.execute(.publication_plan(), out = "D"),
               "Invalid plan publication status")
  expect_identical(state$revoked, "a")
})

test_that("rollback failures remain technical errors under skip", {
  state <- .publication_backend(list(a = .publication_status()),
                                retrieve_error = TRUE, revoke_error = TRUE)
  expect_error(ds.omop.plan.execute(.publication_plan(), out = "D",
                                    on_small_site = "skip"),
               "Cleanup failed: Execution revocation could not be proven")
})

test_that("factor harmonization uses only retained sites and published components", {
  statuses <- list(a = .publication_status(c(data = "D", small = "S")),
                   b = .publication_status(c(data = "D.1"), "partial",
                                           list(small = .publication_rejection())),
                   c = .publication_status(character(), "refused",
                                           list(data = .publication_rejection())))
  calls <- list()
  testthat::local_mocked_bindings(
    .harmonizeOneSymbol = function(sym, conns) {
      calls[[length(calls) + 1L]] <<- list(sym = sym, sites = names(conns))
    }, .package = "dsOMOPClient"
  )
  dsOMOPClient:::.harmonizePublicationFactors(
    lapply(statuses, function(x) if (x$status == "refused") NULL else x$published),
    list(a = "A", b = "B", c = "C")
  )
  expect_identical(calls[[1L]]$sym, c(a = "D", b = "D.1"))
  expect_identical(calls[[1L]]$sites, c("a", "b"))
  expect_identical(calls[[2L]]$sites, "a")
})


test_that("status cleanup failure still revokes every execution", {
  state <- .publication_backend(list(a = .publication_status(),
                                     b = .publication_status()), remove_error = TRUE)
  expect_error(ds.omop.plan.execute(.publication_plan(), out = "D"),
               "Cleanup failed: status removal failed")
  expect_setequal(state$revoked, c("a", "b"))
  expect_false(any(vapply(state$symbols, function(x) "D" %in% x, logical(1))))
})

test_that("factor failures revoke published artifacts federation-wide", {
  state <- .publication_backend(list(a = .publication_status(),
                                     b = .publication_status()))
  testthat::local_mocked_bindings(
    .harmonizePublicationFactors = function(...) stop("factor transport failed"),
    .package = "dsOMOPClient"
  )
  plan <- .publication_plan()
  plan$options$factor_concepts <- TRUE
  plan$options$translate_concepts <- TRUE
  expect_error(ds.omop.plan.execute(plan, out = "D"), "factor transport failed")
  expect_setequal(state$revoked, c("a", "b"))
  expect_true(all(lengths(state$symbols) == 0L))
})

test_that("status validation checks closed fields and root accounting", {
  good <- .publication_status()
  expect_invisible(dsOMOPClient:::.plan_validate_status(good, "data"))
  bad <- good
  bad$published <- list()
  expect_error(dsOMOPClient:::.plan_validate_status(bad, "data"), "Invalid")
  bad <- good
  bad$published$data$data$n_columns <- 2.5
  expect_error(dsOMOPClient:::.plan_validate_status(bad, "data"), "Invalid")
  bad <- good
  bad$published$data$data$support <- 10L
  expect_error(dsOMOPClient:::.plan_validate_status(bad, "data"), "Invalid")
})

test_that("recipe forwards its final-support policy", {
  seen <- NULL
  testthat::local_mocked_bindings(
    ds.omop.plan.execute = function(..., on_small_site) {
      seen <<- on_small_site
      invisible(NULL)
    }, .package = "dsOMOPClient"
  )
  recipe <- omop_recipe(variables = omop_variable_age(),
                        outputs = omop_output(name = "data", type = "wide"))
  recipe_execute(recipe, on_small_site = "skip")
  expect_identical(seen, "skip")
})


test_that("a warning promoted to an error still rolls back before status deletion", {
  state <- .publication_backend(list(
    a = .publication_status(c(data = "D"), "partial",
                            list(small = .publication_rejection()))
  ))
  withr::local_options(warn = 2)
  expect_error(ds.omop.plan.execute(.publication_plan(c("data", "small")),
                                    out = c(data = "D", small = "S")),
               "Plan publication is partial")
  expect_identical(state$revoked, "a")
  expect_length(state$symbols$a, 0L)
})

test_that("renamed component factors share one level specification", {
  assigned <- list()
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, success, ...) {
      site <- names(conns)[[1L]]
      expect_identical(as.character(expr[[2L]]), if (site == "a") "D" else "D.1")
      report <- list(levels = list(gender_concept_id = if (site == "a") "8507" else "8532"),
                     unsafe = character(), nfilter_levels_max = 40L)
      success(site, report)
      stats::setNames(list(report), site)
    },
    datashield.assign.expr = function(conns, symbol, expr, success, ...) {
      site <- names(conns)[[1L]]
      assigned[[site]] <<- list(symbol = symbol, spec = expr[[3L]])
      success(site)
      invisible(NULL)
    }, .package = "DSI"
  )
  dsOMOPClient:::.harmonizeOneSymbol(c(a = "D", b = "D.1"), list(a = "A", b = "B"))
  expect_identical(assigned$a$symbol, "D")
  expect_identical(assigned$b$symbol, "D.1")
  expect_identical(assigned$a$spec, assigned$b$spec)
  expect_identical(assigned$a$spec,
                   dsOMOPClient:::.ds_encode(list(gender_concept_id = c("8507", "8532"))))
})


test_that("legacy composite aliases preserve the successful return mapping", {
  status <- .publication_status()
  status$published$data <- list(
    events = list(symbol = "D.events", grain = "episode", n_columns = 5L),
    risk_sets = list(symbol = "D.riskSets", grain = "episode", n_columns = 3L)
  )
  .publication_backend(list(a = status, b = status))
  expect_identical(ds.omop.plan.execute(.publication_plan(), out = "D"),
                   c(data = "D"))
})

test_that("status validation admits the closed reason only with validation_error (A-18)", {
  entry <- function(...) list(code = "validation_error",
                              message = "Publication refused: validation_error.", ...)
  good <- .publication_status(character(), "refused",
                              list(data = entry(reason = "adapter_person_mismatch")))
  expect_invisible(dsOMOPClient:::.plan_validate_status(good, "data"))
  good$rejected$data$reason <- "cost_currency_mixed"
  expect_invisible(dsOMOPClient:::.plan_validate_status(good, "data"))
  # A-20 (j): the interval helpers' bounded realisations
  for (reason in c("stockpile_row_cap", "reducer_row_cap")) {
    ok <- good; ok$rejected$data$reason <- reason
    expect_invisible(dsOMOPClient:::.plan_validate_status(ok, "data"))
  }
  bad <- good; bad$rejected$data$reason <- "unit_unknown"
  expect_error(dsOMOPClient:::.plan_validate_status(bad, "data"), "Invalid")
  bad <- good; bad$rejected$data$reason <- 1L
  expect_error(dsOMOPClient:::.plan_validate_status(bad, "data"), "Invalid")
  bad <- good; bad$rejected$data$reason <- NA_character_
  expect_error(dsOMOPClient:::.plan_validate_status(bad, "data"), "Invalid")
  bad <- good; bad$rejected$data$code <- "unavailable_table"
  expect_error(dsOMOPClient:::.plan_validate_status(bad, "data"), "Invalid")
  bad <- good; bad$rejected$data$field <- "events/members"
  expect_error(dsOMOPClient:::.plan_validate_status(bad, "data"), "Invalid")
})

test_that("the refusal diagnostic names the validated reason with the output and code (A-18)", {
  for (reason in c("adapter_person_mismatch", "cost_currency_mixed", "stockpile_row_cap",
                   "reducer_row_cap")) {
    for (policy in c("error", "skip")) {
      statuses <- list(a = .publication_status(),
                       b = .publication_status(character(), "refused", list(data = list(
                         code = "validation_error",
                         message = "Publication refused: validation_error.",
                         reason = reason))))
      state <- .publication_backend(statuses)
      expect_error(ds.omop.plan.execute(.publication_plan(), out = "D", on_small_site = policy),
                   paste0("b:data [validation_error] (", reason, ")"), fixed = TRUE)
      expect_setequal(state$revoked, c("a", "b"))
      expect_true(all(lengths(state$symbols) == 0L))
    }
  }
  # a reason paired with another code is a malformed status: refused and rolled back
  statuses <- list(a = .publication_status(),
                   b = .publication_status(character(), "refused", list(data = list(
                     code = "unavailable_table",
                     message = "Publication refused: unavailable_table.",
                     reason = "cost_currency_mixed"))))
  state <- .publication_backend(statuses)
  expect_error(ds.omop.plan.execute(.publication_plan(), out = "D", on_small_site = "skip"),
               "Invalid plan publication status")
  expect_setequal(state$revoked, c("a", "b"))
})
