# WS-8 (design-2.10.3 sections 4.4, 5, 7.3, 10.1): capability negotiation and
# v1/v2 routing, and language-2 plans executed through the one publication
# protocol of .plan_execute_impl() (status, inventory, commit policy and
# rollback, whose full strict/non-strict x error/skip matrix is pinned by
# test-publication-status.R).

# A real session object in the client environment, for tests that stop before
# any server call (negotiation, routing).
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

.v2_recipe <- function(publish = NULL) {
  mi <- omop_event("condition_occurrence", 4329847, name = "mi")
  omop_recipe(
    populations = omop_population("c1", index = omop_index(list(cohort_definition_id = 1), "all",
                                                          end = omop_end("event_end"))),
    variables = list(sex = omop_sex()),
    outputs = list(
      omop_output("baseline", rows = "c1", format = "wide"),
      omop_output("tte", rows = "c1", format = "survival", options = list(
        outcomes = list(mi = mi), censor = list("observation_end"),
        tars = list(itt = omop_window(1, NULL), short = omop_window(1, 30))))),
    publish = publish)
}
.legacy_recipe <- function() {
  omop_recipe(variables = omop_variable_sex(), outputs = omop_output("w", type = "wide"))
}

.component <- function(symbol, grain = "episode", n = 5L) {
  list(symbol = symbol, grain = grain, n_columns = n)
}
.v2_status <- function(baseline = "D_baseline", tte = TRUE, status = "ok",
                       rejected = list()) {
  published <- list()
  if (!is.null(baseline)) published$baseline <- list(data = .component(baseline))
  if (isTRUE(tte)) {
    published$tte <- list(itt = .component("D_tte.itt"), short = .component("D_tte.short"))
  }
  list(status = status, published = published, rejected = rejected,
       unavailable = character(0))
}

test_that("a 2.10.1-only recipe executes as its unchanged v1 plan", {
  state <- .publication_backend(list(srv1 = .publication_status(c(w = "D_w"))))
  .with_caps(list(srv1 = list()))            # no recipe_language block needed
  log <- .capture_exec()
  expect_identical(recipe_execute(.legacy_recipe()), c(w = "D_w"))
  sent <- .decode(log$exec[[3L]])
  expect_null(sent$version)                     # a v1 plan
  expect_identical(sent$outputs$w$type, "person_level")
  expect_identical(as.list(log$exec)$commit_policy, "server")
  expect_identical(state$calls[[1L]]$method, "omopPlanStatusDS")
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

test_that("a v2 plan runs through the publication protocol, sent losslessly", {
  state <- .publication_backend(list(a = .v2_status(), b = .v2_status()))
  .with_caps(list(a = .caps(), b = .caps()))
  log <- .capture_exec()
  res <- recipe_execute(.v2_recipe())
  sent <- .decode(log$exec[[3L]])
  expect_identical(sent$version, 2L)
  expect_named(sent$outputs, c("baseline", "tte"))
  expect_identical(as.list(log$exec)$commit_policy, "server")
  expect_identical(state$calls[[1L]]$method, "omopPlanStatusDS")
  # one symbol per survival TAR, recognised as the ordinary mapping
  expect_identical(res, c(baseline = "D_baseline", tte = "D_tte"))
  expect_length(state$revoked, 0L)
})

test_that("fresh names on collision return the per-site inventory (v2 plan)", {
  state <- .publication_backend(list(a = .v2_status("D_baseline.1"), b = .v2_status()),
                                initial = list(a = "D_baseline", b = character()))
  .with_caps(list(a = .caps(), b = .caps()))
  res <- recipe_execute(.v2_recipe())
  expect_identical(res$a$baseline$data$symbol, "D_baseline.1")
  expect_identical(res$b$baseline$data$symbol, "D_baseline")
  expect_identical(res$a$tte$itt$symbol, "D_tte.itt")
})

test_that("on_small_site = 'skip' asks for all-or-none and returns NULL for a refused site", {
  refused <- .v2_status(baseline = NULL, tte = FALSE, status = "refused",
                        rejected = list(baseline = .publication_rejection(),
                                        tte = .publication_rejection()))
  state <- .publication_backend(list(a = .v2_status(), b = refused))
  .with_caps(list(a = .caps(), b = .caps()))
  log <- .capture_exec()
  res <- recipe_execute(.v2_recipe(), on_small_site = "skip")
  expect_identical(as.list(log$exec)$commit_policy, "all_or_none")
  expect_true("b" %in% names(res))
  expect_null(res$b)
  expect_identical(res$a$baseline$data$symbol, "D_baseline")
  # under the default a strict refusal rolls every site back
  state <- .publication_backend(list(a = .v2_status(), b = refused))
  .with_caps(list(a = .caps(), b = .caps()))
  expect_error(recipe_execute(.v2_recipe()), "b:baseline \\[insufficient_support\\]")
  expect_setequal(state$revoked, c("a", "b"))
})

test_that("only the publish roots are requested and validated (B1)", {
  state <- .publication_backend(list(a = .v2_status(tte = FALSE)))
  .with_caps(list(a = .caps()))
  log <- .capture_exec()
  res <- recipe_execute(.v2_recipe(publish = "baseline"))
  expect_identical(res, c(baseline = "D_baseline"))
  expect_identical(names(.decode(log$exec[[4L]])), "baseline")
  # a server reporting the private output is refused and the execution revoked
  state <- .publication_backend(list(a = .v2_status()))
  .with_caps(list(a = .caps()))
  expect_error(recipe_execute(.v2_recipe(publish = "baseline")),
               "Invalid plan publication status")
  expect_identical(state$revoked, "a")
})

test_that("a 2.10.1-style recipe the v1 compiler refuses as newly permitted runs as its v2 plan", {
  # design 5: newly permitted shapes run only where every server speaks
  # language 2; every other v1 refusal stands.
  r <- omop_recipe(variables = omop_variable_age(),
                   filters = omop_filter_group(omop_filter_sex("F"), operator = "NOT"),
                   outputs = omop_output(name = "age", type = "wide"))
  conns <- .with_servers(list(a = .caps(), b = .caps()))
  expect_s3_class(dsOMOPClient:::.recipe_plan_for_session(r, "omop", conns), "omop_plan_v2")
  plain <- omop_recipe(variables = omop_variable_age(), filters = omop_filter_sex("F"),
                       outputs = omop_output(name = "age", type = "wide"))
  expect_s3_class(dsOMOPClient:::.recipe_plan_for_session(plain, "omop", conns), "omop_plan")
  common <- omop_recipe(
    variables = list(omop_variable(name = "a", table = "measurement", concept_id = 1,
                                   format = "count", time_window = list(start = -90, end = 0))),
    outputs = omop_output(name = "f", type = "features"))
  expect_error(dsOMOPClient:::.recipe_plan_for_session(common, "omop", conns), "grain")
  conns <- .with_servers(list(a = .caps(), old = list()))
  expect_error(dsOMOPClient:::.recipe_plan_for_session(r, "omop", conns),
               "no v1 rendering.*older than dsOMOP 2.10.3")
})
