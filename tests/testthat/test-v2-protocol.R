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

# Inspect the expression after the same deparse/parse boundary used by the
# transport. A bare named scalar becomes c(name = "symbol") at this boundary,
# even when inspecting the original in-memory call would hide that defect.
.protocol_wire_mapping <- function(expr, expected) {
  wire <- parse(text = paste(deparse(expr), collapse = "\n"))[[1L]]
  expect_false(any(vapply(as.list(wire)[-1L], is.call, logical(1))))
  token <- wire[[4L]]
  expect_type(token, "character")
  expect_length(token, 1L)
  expect_null(names(token))
  expect_match(token, "^B64:")
  decoded <- .decode_ds(token)
  if (length(expected)) expect_identical(decoded, as.list(expected)) else {
    expect_type(decoded, "list")
    expect_length(decoded, 0L)
    expect_identical(token, "B64:e30") # {}, not []
  }
  invisible(wire)
}

test_that("v1 singleton, multiple and subset mappings survive strict current and legacy transport", {
  single <- .legacy_recipe_b1()
  multiple <- omop_recipe(
    variables = omop_variable_age(name = "age_at_2025", year = 2025),
    outputs = list(omop_output("a", type = "wide"), omop_output("b", type = "wide")))
  cases <- list(
    list(recipe = single, out = c(ages = "D_single")),
    list(recipe = multiple, out = c(a = "D_first", b = "D_second")),
    list(recipe = multiple, out = c(b = "D_subset")))
  for (sites in list(list(new = "current"), list(old = "old"),
                    list(old = "old", new = "current"))) {
    for (dp in c(FALSE, TRUE)) for (case in cases) {
      state <- do.call(.legacy_federation, sites)
      result <- suppressWarnings(if (dp) {
        ds.omop.dp.prepare(case$recipe, out = case$out)
      } else recipe_execute(case$recipe, out = case$out))
      expect_identical(result, case$out)
      for (site in names(state$sent)) {
        wire <- .protocol_wire_mapping(state$sent[[site]], case$out)
        expect_identical(as.character(wire[[1L]]),
                         if (dp) "omopDpPlanExecuteDS" else "omopPlanExecuteDS")
        if (length(sites$old)) {
          expect_null(as.list(wire)$commit_policy)
          expect_null(.check_2102_call(.contract_2102(), wire))
        } else expect_identical(as.list(wire)$commit_policy, "server")
      }
      if (length(sites$old)) {
        expect_false(any(c("omopPlanStatusDS", "omopStagedRevokeDS") %in% .called(state)))
      }
    }
  }
})

test_that("native mappings preserve exact publication roots including the empty object", {
  single <- recipe_to_plan(.v2_recipe_b1())
  multiple <- recipe_to_plan(omop_recipe(variables = list(age = omop_age()),
    outputs = list(omop_output("o", format = "wide"), omop_output("other", format = "wide"))))
  empty <- multiple
  empty$publish <- list()
  cases <- list(
    list(plan = single, out = c(o = "D_single")),
    list(plan = multiple, out = c(o = "D_first", other = "D_second")),
    list(plan = multiple, out = c(other = "D_subset")),
    list(plan = empty, out = stats::setNames(character(0), character(0))))
  for (dp in c(FALSE, TRUE)) for (case in cases) {
    state <- .legacy_federation(new = "current")
    result <- if (dp) ds.omop.dp.prepare(case$plan, out = case$out) else
      ds.omop.plan.execute(case$plan, out = case$out)
    expect_identical(result, case$out)
    wire <- .protocol_wire_mapping(state$sent$current, case$out)
    sent <- .decode_ds(wire[[3L]])
    expect_identical(as.character(unlist(sent$publish, use.names = FALSE)), names(case$out))
    expect_identical(as.list(wire)$commit_policy, "server")
    expect_true("omopPlanStatusDS" %in% .called(state))
  }
})

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
  transported <- parse(text = paste(deparse(sent), collapse = "\n"))[[1L]]
  expect_false(any(vapply(as.list(transported)[-1L], is.call, logical(1))))
  expect_identical(.decode_ds(transported[[4L]]), list(ages = "D_ages"))
  expect_identical(transported[[2L]], sent[[2L]])
  expect_identical(transported[[5L]], "memory")
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

test_that("the legacy route checks the out mapping as 2.10.2 did, before any assignment", {
  state <- .legacy_federation(old = "old", new = "new")
  r <- omop_recipe(variables = omop_variable_age(name = "age_at_2025", year = 2025),
                   outputs = list(omop_output("a", type = "wide"), omop_output("b", type = "wide")))
  expect_error(suppressWarnings(recipe_execute(r, out = c(a = "X", b = "X.covariates"))),
               "overlapping reserved component families")
  expect_error(suppressWarnings(recipe_execute(r, out = c(a = "X", c = "Y"))),
               "out names not among plan outputs")
  expect_identical(state$assigns, c(old = 0L, new = 0L))
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

test_that("a v1 skip request still uses the coordinator's legacy route everywhere", {
  state <- .legacy_federation(old = "old", new = "new")
  expect_warning(out <- recipe_execute(.legacy_recipe_b1(), on_small_site = "skip"),
                 "publication status, on_small_site.*do not apply")
  expect_identical(out, c(ages = "D_ages"))
  expect_identical(state$assigns, c(old = 1L, new = 1L))
  for (s in c("old", "new")) expect_null(as.list(state$sent[[s]])$commit_policy)
  expect_false(any(c("omopPlanStatusDS", "omopStagedRevokeDS") %in% .called(state)))
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

test_that("preflight checks methods on the final site before any assignment", {
  for (method in c("omopPlanExecuteDS", "omopFactorLevelsDS", "omopAsFactorColumnsDS")) {
    state <- .legacy_federation(new = c("first", "last"))
    methods <- .current_published_methods(list(first = "first", last = "last"))
    methods$last$assign <- setdiff(methods$last$assign, method)
    methods$last$aggregate <- setdiff(methods$last$aggregate, method)
    local_mocked_bindings(.dsomop_published_methods = function(...) methods,
                          .package = "dsOMOPClient")
    expect_error(recipe_execute(.legacy_recipe_b1()), paste0("last lacks ", method))
    expect_identical(state$assigns, c(first = 0L, last = 0L))
  }
})

test_that("an unlistable site's profile stops all assignments", {
  state <- .legacy_federation(new = c("first", "last"))
  methods <- .current_published_methods(list(first = "first", last = "last"))
  methods$last$error <- "profile unavailable"
  local_mocked_bindings(.dsomop_published_methods = function(...) methods,
                        .package = "dsOMOPClient")
  expect_error(recipe_execute(.legacy_recipe_b1()),
               "Cannot check.*last.*profile unavailable.*nothing was sent")
  expect_identical(state$assigns, c(first = 0L, last = 0L))
})

test_that("DP preflight needs its assign method but no factor recoding methods", {
  state <- .legacy_federation(new = c("first", "last"),
                              unpublished = "omopDpPlanExecuteDS")
  expect_error(ds.omop.dp.prepare(.legacy_recipe_b1()), "lacks omopDpPlanExecuteDS")
  expect_identical(state$assigns, c(first = 0L, last = 0L))
  state <- .legacy_federation(new = c("first", "last"),
                              unpublished = c("omopFactorLevelsDS", "omopAsFactorColumnsDS"))
  expect_silent(out <- ds.omop.dp.prepare(.legacy_recipe_b1()))
  expect_identical(out, c(ages = "D_ages"))
  expect_identical(state$assigns, c(first = 1L, last = 1L))
})

test_that("legacy fallback names every old site exactly once", {
  state <- .legacy_federation(old = c("old_a", "old_b"), new = "new")
  warnings <- character(0)
  withCallingHandlers(recipe_execute(.legacy_recipe_b1()), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_length(warnings, 1L)
  expect_match(warnings, "^dsOMOP on old_a, old_b predates")
  expect_identical(state$assigns, c(old_a = 1L, old_b = 1L, new = 1L))
  expect_false(any(c("omopPlanStatusDS", "omopStagedRevokeDS") %in% .called(state)))
})

test_that("empty publication roots need no unused factor methods", {
  state <- .legacy_federation(new = "a",
                              unpublished = c("omopFactorLevelsDS", "omopAsFactorColumnsDS"))
  plan <- recipe_to_plan(.v2_recipe_b1())
  plan$publish <- list()
  expect_silent(out <- ds.omop.plan.execute(plan))
  expect_identical(out, stats::setNames(character(0), character(0)))
  expect_identical(state$assigns, c(a = 1L))
  expect_false("omopFactorLevelsDS" %in% .called(state))
  expect_true("omopPlanStatusDS" %in% .called(state))
})
