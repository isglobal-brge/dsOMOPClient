.publication_plan <- function(roots = "data") {
  plan <- ds.omop.plan()
  for (root in roots) plan <- ds.omop.plan.baseline(plan, name = root)
  ds.omop.plan.options(plan, factor_concepts = FALSE)
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
