# WS-8 (review B1; design 4.4, 7.3, A-14): before anything is assigned, every
# participating site is checked for the publication protocol. A language-2
# plan needs it on every site; a v1 plan sent to a federation with a dsOMOP
# 2.10.2 site runs as the 2.10.2 legacy call on every site. The 2.10.2 sites
# here publish the real 2.10.2 method inventory and enforce its argument
# contract (helper-legacy-servers.R).

.legacy_recipe_b1 <- function() omop_recipe(
  variables = omop_variable_age(name = "age_at_2025", year = 2025),
  outputs = omop_output("ages", type = "wide"))
.v2_recipe_b1 <- function() omop_recipe(variables = list(age = omop_age()),
                                        outputs = omop_output("o", format = "wide"))

test_that("the fixture reproduces the 2.10.2 contract: commit_policy is refused", {
  contract <- .contract_2102()
  expect_error(contract$.omopAnalysisScopeFromDots(NULL, list(commit_policy = "server")),
               "Unexpected analysis argument name")
  m <- .methods_2102()
  expect_identical(m$version, "2.10.2")
  expect_true("omopPlanExecuteDS" %in% m$assign)
  expect_false(any(c("omopPlanStatusDS", "omopStagedRevokeDS") %in% m$aggregate))
})

test_that("a single 2.10.2 site runs a v1 plan as the 2.10.2 legacy call", {
  state <- .legacy_federation(old = "old")
  r <- .legacy_recipe_b1()
  expect_warning(res <- recipe_execute(r),
                 "old predates the publication protocol.*legacy call.*do not apply")
  expect_identical(res, c(ages = "D_ages"))
  expect_identical(state$assigns, c(old = 1L))
  sent <- state$sent$old
  expect_identical(as.character(sent[[1L]]), "omopPlanExecuteDS")
  expect_null(as.list(sent)$commit_policy)
  # the same v1 payload, byte for byte
  expect_identical(sent[[3L]], dsOMOPClient:::.ds_encode(recipe_to_plan(r, version = 1)))
  expect_false(any(c("omopPlanStatusDS", "omopStagedRevokeDS") %in% .called(state)))
  expect_true("D_ages" %in% state$symbols$old)
})

test_that("a mixed federation sends the legacy call to every site, with one warning", {
  state <- .legacy_federation(old = "old", new = "new")
  warnings <- character(0)
  res <- withCallingHandlers(recipe_execute(.legacy_recipe_b1()), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_identical(res, c(ages = "D_ages"))
  expect_length(warnings, 1L)
  expect_match(warnings, "^dsOMOP on old predates")   # only the old site is named
  expect_identical(state$assigns, c(old = 1L, new = 1L))
  for (s in c("old", "new")) expect_null(as.list(state$sent[[s]])$commit_policy)
  expect_false(any(c("omopPlanStatusDS", "omopStagedRevokeDS") %in% .called(state)))
})

test_that("a language-2 plan is refused before any assignment on any site", {
  for (sites in list(list(old = "old"), list(old = "old", new = "new"))) {
    state <- do.call(.legacy_federation, sites)
    err <- tryCatch(recipe_execute(.v2_recipe_b1()), error = conditionMessage)
    expect_match(err, "old lacks omopPlanStatusDS, omopStagedRevokeDS")
    expect_match(err, "installs dsOMOP 2.10.4 on those servers and publishes its methods")
    expect_match(err, "Nothing was sent to any site")
    expect_identical(sum(state$assigns), 0L, info = paste(names(sites), collapse = "+"))
    # and directly through the executor
    plan <- recipe_to_plan(.v2_recipe_b1())
    expect_error(ds.omop.plan.execute(plan), "old lacks omopPlanStatusDS")
    expect_identical(sum(state$assigns), 0L)
  }
})

test_that("on_small_site = 'skip' needs the protocol on every site", {
  state <- .legacy_federation(old = "old", new = "new")
  expect_error(recipe_execute(.legacy_recipe_b1(), on_small_site = "skip"),
               "on_small_site = \"skip\" needs the publication protocol")
  expect_identical(sum(state$assigns), 0L)
})

test_that("DP preparation follows the same rule", {
  state <- .legacy_federation(old = "old")
  expect_warning(dp <- ds.omop.dp.prepare(.legacy_recipe_b1()), "legacy call")
  expect_identical(dp, c(ages = "D_ages"))
  expect_identical(state$assigns, c(old = 1L))
  expect_identical(as.character(state$sent$old[[1L]]), "omopDpPlanExecuteDS")
  expect_null(as.list(state$sent$old)$commit_policy)
  state <- .legacy_federation(old = "old", new = "new")
  expect_error(ds.omop.dp.prepare(.v2_recipe_b1()), "old lacks omopPlanStatusDS")
  expect_identical(sum(state$assigns), 0L)
})

test_that("a federation of current sites uses the publication protocol", {
  state <- .legacy_federation(new = c("a", "b"))
  expect_silent(res <- recipe_execute(.legacy_recipe_b1()))
  expect_identical(res, c(ages = "D_ages"))
  expect_identical(state$assigns, c(a = 1L, b = 1L))
  for (s in c("a", "b")) expect_identical(as.list(state$sent[[s]])$commit_policy, "server")
  expect_true("omopPlanStatusDS" %in% .called(state))
})

test_that("a site that does not publish the execution method is refused before assignment", {
  state <- .legacy_federation(new = c("a", "b"), unpublished = "omopPlanExecuteDS")
  expect_error(recipe_execute(.legacy_recipe_b1()),
               "do not publish the methods this execution calls.*a lacks omopPlanExecuteDS")
  expect_identical(sum(state$assigns), 0L)
})

test_that("explain reports the same execution decision", {
  state <- .legacy_federation(old = "old", new = "new")
  ex <- recipe_explain(.v2_recipe_b1(), conns = list(old = "old", new = "new"))
  expect_match(ex$refusal, "old lacks omopPlanStatusDS")
  ex1 <- recipe_explain(.legacy_recipe_b1(), conns = list(old = "old", new = "new"))
  expect_null(ex1$refusal)
  expect_match(paste(ex1$text, collapse = "\n"), "Execution would run the legacy call")
  expect_identical(sum(state$assigns), 0L)
})
