# WS-8 (review B1; design 4.4, 7.3, A-14): before anything is assigned, every
# participating site is checked for the publication protocol. A language-2
# plan needs it on every site. Since 2.10.6, v1 publication also requires
# concept identity support; older servers are refused before assignment.
# The old-site fixture retains the actual 2.10.2 argument contract.

.legacy_recipe_b1 <- function() omop_recipe(
  variables = omop_variable_age(name = "age_at_2025", year = 2025),
  outputs = omop_output("ages", type = "wide"),
  options = list(translate_concepts = FALSE))
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

test_that("v1 singleton, multiple and subset mappings survive strict current transport", {
  single <- .legacy_recipe_b1()
  multiple <- omop_recipe(
    variables = omop_variable_age(name = "age_at_2025", year = 2025),
    outputs = list(omop_output("a", type = "wide"), omop_output("b", type = "wide")),
    options = list(translate_concepts = FALSE))
  cases <- list(
    list(recipe = single, out = c(ages = "D_single")),
    list(recipe = multiple, out = c(a = "D_first", b = "D_second")),
    list(recipe = multiple, out = c(b = "D_subset")))
  for (sites in list(list(new = "current"))) {
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

test_that("a v1 plan requires concept identity support even for untranslated labels", {
  for (translate in c(FALSE, TRUE)) {
    state <- .legacy_federation(old = "old")
    r <- recipe_set_options(.legacy_recipe_b1(), translate_concepts = translate)
    expect_error(recipe_execute(r), "Concept factors require server support.*output.concept_labels.*old")
    expect_identical(state$assigns, c(old = 0L))
    expect_length(state$symbols$old, 0L)
  }
})

test_that("a mixed federation refuses missing concept identity support before assignment", {
  state <- .legacy_federation(old = "old", new = "new")
  expect_error(recipe_execute(.legacy_recipe_b1()), "output.concept_labels on: old")
  expect_identical(state$assigns, c(old = 0L, new = 0L))
})

test_that("v1 checks the out mapping before any assignment", {
  state <- .legacy_federation(new = c("old", "new"))
  r <- omop_recipe(variables = omop_variable_age(name = "age_at_2025", year = 2025),
                   outputs = list(omop_output("a", type = "wide"), omop_output("b", type = "wide")),
    options = list(translate_concepts = FALSE))
  expect_error(suppressWarnings(recipe_execute(r, out = c(a = "X", b = "X"))),
               "unique")
  expect_error(suppressWarnings(recipe_execute(r, out = c(a = "X", c = "Y"))),
               "out names not among plan outputs")
  expect_identical(state$assigns, c(old = 0L, new = 0L))
})

test_that("a language-2 plan is refused before any assignment on any site", {
  for (sites in list(list(old = "old"), list(old = "old", new = "new"))) {
    state <- do.call(.legacy_federation, sites)
    err <- tryCatch(recipe_execute(.v2_recipe_b1()), error = conditionMessage)
    expect_match(err, "old lacks omopPlanStatusDS, omopStagedRevokeDS")
    expect_match(err, "installs dsOMOP 2.10.3 on those servers and publishes its methods")
    expect_match(err, "Nothing was sent to any site")
    expect_identical(sum(state$assigns), 0L, info = paste(names(sites), collapse = "+"))
    # and directly through the executor
    plan <- recipe_to_plan(.v2_recipe_b1())
    expect_error(ds.omop.plan.execute(plan), "output.concept_labels on: old")
    expect_identical(sum(state$assigns), 0L)
  }
})

test_that("a v1 skip request cannot bypass mandatory concept identity support", {
  state <- .legacy_federation(old = "old", new = "new")
  expect_error(recipe_execute(.legacy_recipe_b1(), on_small_site = "skip"), "output.concept_labels on: old")
  expect_identical(state$assigns, c(old = 0L, new = 0L))
})

test_that("DP preparation follows the same rule", {
  state <- .legacy_federation(old = "old")
  expect_error(ds.omop.dp.prepare(.legacy_recipe_b1()), "output.concept_labels on: old")
  expect_identical(state$assigns, c(old = 0L))
  state <- .legacy_federation(old = "old", new = "new")
  expect_error(ds.omop.dp.prepare(.v2_recipe_b1()), "output.concept_labels on: old")
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
  expect_match(ex1$refusal, "output.concept_labels on: old")
  expect_match(paste(ex1$text, collapse = "\n"), "Concept factors require server support")
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
    expect_error(recipe_execute(recipe_set_options(.legacy_recipe_b1(),
      translate_concepts = TRUE)), paste0("last lacks ", method))
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

test_that("DP preflight requires the common factor reconciliation methods", {
  state <- .legacy_federation(new = c("first", "last"),
                              unpublished = "omopDpPlanExecuteDS")
  expect_error(ds.omop.dp.prepare(.legacy_recipe_b1()), "lacks omopDpPlanExecuteDS")
  expect_identical(state$assigns, c(first = 0L, last = 0L))
  state <- .legacy_federation(new = c("first", "last"),
                              unpublished = c("omopFactorLevelsDS", "omopAsFactorColumnsDS"))
  expect_error(ds.omop.dp.prepare(.legacy_recipe_b1()), "lacks omopAsFactorColumnsDS|lacks omopFactorLevelsDS")
  expect_identical(state$assigns, c(first = 0L, last = 0L))
})

test_that("the concept support refusal names every unsupported site", {
  state <- .legacy_federation(old = c("old_a", "old_b"), new = "new")
  expect_error(recipe_execute(.legacy_recipe_b1()), "output.concept_labels on: old_a, old_b")
  expect_identical(state$assigns, c(old_a = 0L, old_b = 0L, new = 0L))
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


test_that("both recipe languages refuse 2.10.5 before any publication", {
  state <- .legacy_federation(new = "site")
  session <- dsOMOPClient:::.get_session()
  session$server_versions <- c(site = "2.10.5")
  local_mocked_bindings(.get_session = function(...) session, .package = "dsOMOPClient")
  for (plan in list(recipe_to_plan(.legacy_recipe_b1(), version = 1),
                    recipe_to_plan(.v2_recipe_b1()))) {
    expect_error(ds.omop.plan.execute(plan), "2.10.6 or later on: site")
    expect_identical(state$assigns, c(site = 0L))
  }
  ex <- recipe_explain(.legacy_recipe_b1(), conns = list(site = "site"))
  expect_match(ex$refusal, "2.10.6 or later on: site")
})
