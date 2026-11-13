# WS-8 fix round (Astra's review of 12 November 2026): regressions for B2, B3,
# B4, B6, N1, N2 and the operation-inventory parity with the server.

.server_ops <- function(plan) {
  sort(unique(dsOMOP:::.irOperations(dsOMOP:::.planNormalizeV2(dsOMOPClient:::.v2_plain(plan)))))
}
.server_semantic_errors <- function(plan) {
  dsOMOP:::.irSemanticErrors(dsOMOP:::.planNormalizeV2(dsOMOPClient:::.v2_plain(plan)))
}

# --- B2 -------------------------------------------------------------------------
.observed_recipe <- function(gap) omop_recipe(
  populations = omop_population("p", filters = list(omop_observed(before = 365, gap = gap))),
  variables = list(sex = omop_sex()), outputs = omop_output("w", rows = "p", format = "wide"))

test_that("an observed criterion with a gap needs population.coverage.union (B2)", {
  ops <- function(gap) dsOMOPClient:::.plan_v2_operations(recipe_to_plan(.observed_recipe(gap)))
  expect_false("population.coverage.union" %in% ops(NULL))
  expect_true("population.coverage.union" %in% ops(0))
  expect_true("population.coverage.union" %in% ops(30))
  # refused before any assignment when a server lacks the operation
  state <- .legacy_federation(new = "a", ops = setdiff(.V2_ALL_OPS, "population.coverage.union"))
  expect_error(recipe_execute(.observed_recipe(0)), "a lacks population.coverage.union")
  expect_identical(sum(state$assigns), 0L)
  skip_if_not_installed("dsOMOP")
  for (gap in list(NULL, 0, 30)) {
    p <- recipe_to_plan(.observed_recipe(gap))
    expect_identical(as.character(dsOMOPClient:::.plan_v2_operations(p)), .server_ops(p))
  }
})

test_that("the client operation inventory equals the server's (criteria and corpus)", {
  skip_if_not_installed("dsOMOP")
  mi <- omop_event("condition_occurrence", 4329847, name = "mi")
  rx <- omop_event("drug_exposure", 1, name = "rx")
  plans <- list(
    age = recipe_to_plan(omop_recipe(
      populations = omop_population("p", index = omop_index(mi), filters = list(omop_age_is(min = 18))),
      variables = list(sex = omop_sex()),
      outputs = omop_output("w", rows = "p", format = "wide", grain = "episode"))),
    not = recipe_to_plan(omop_recipe(
      populations = omop_population("p", filters = list(omop_not(omop_has(mi)))),
      variables = list(sex = omop_sex()), outputs = omop_output("w", rows = "p", format = "wide"))),
    anchor = recipe_to_plan(omop_recipe(
      populations = omop_population("p", index = omop_index(mi)),
      variables = list(d = omop_variable(event = rx, reducer = omop_reduce(
        "days_since", reference = omop_anchor(mi, "first")))),
      outputs = omop_output("w", rows = "p", format = "wide"))),
    union_subset = recipe_to_plan(omop_recipe(
      populations = list(omop_population("p", index = omop_index(mi)),
                         omop_population("q", index = omop_index(rx)),
                         omop_population("u", union = c("p", "q"))),
      variables = list(sex = omop_sex()),
      outputs = list(omop_output("a", rows = "u", format = "wide"),
                     omop_output("b", rows = "p", format = "wide")),
      publish = "a")))
  for (nm in names(plans)) {
    expect_identical(as.character(dsOMOPClient:::.plan_v2_operations(plans[[nm]])),
                     .server_ops(plans[[nm]]), info = nm)
  }
  acc <- system.file("plans", "acceptance", package = "dsOMOP")
  skip_if(!nzchar(acc), "dsOMOP acceptance corpus not installed")
  for (d in list.dirs(acc, recursive = FALSE)) {
    frozen <- jsonlite::fromJSON(file.path(d, "plan.json"), simplifyVector = FALSE)
    expect_identical(as.character(dsOMOPClient:::.plan_v2_operations(frozen)), .server_ops(frozen),
                     info = basename(d))
  }
})

# --- B3 -------------------------------------------------------------------------
.match_recipe <- function(cases_kind = "index", age = FALSE, controls_also = NULL) {
  mi <- omop_event("condition_occurrence", 1, name = "mi")
  pops <- list(omop_population("cases", index = omop_index("mi")),
               omop_population("controls", filters = if (age) list(omop_age_is(min = 18)) else list()))
  cases <- "cases"
  if (identical(cases_kind, "child")) {
    pops <- c(pops, list(omop_population("filtered_cases", parent_id = "cases")))
    cases <- "filtered_cases"
  }
  if (identical(cases_kind, "union")) {
    pops <- c(pops, list(omop_population("more", index = omop_index("mi")),
                         omop_population("both", union = c("cases", "more"))))
    cases <- "both"
  }
  outs <- list(omop_output("m", format = "membership", rows = "matched"))
  if (identical(controls_also, "output")) {
    outs <- c(outs, list(omop_output("c", format = "wide", rows = "controls")))
  }
  if (identical(controls_also, "parent")) {
    pops <- c(pops, list(omop_population("sub", parent_id = "controls")))
    outs <- c(outs, list(omop_output("s", format = "wide", rows = "sub")))
  }
  if (identical(controls_also, "member")) {
    pops <- c(pops, list(omop_population("mix", union = c("controls", "cases"))))
    outs <- c(outs, list(omop_output("x", format = "wide", rows = "mix")))
  }
  if (identical(controls_also, "cases")) {
    pops <- c(pops, list(omop_population("again", match = omop_match("controls", "controls", "mi"))))
  }
  pops <- c(pops, list(omop_population("matched", match = omop_match(cases, "controls", "mi"))))
  omop_recipe(events = list(mi = mi), populations = pops, outputs = outs)
}

test_that("matching accepts inherited and set-op indexed cases and controls-time age (B3)", {
  for (r in list(.match_recipe("child"), .match_recipe("union"), .match_recipe(age = TRUE))) {
    plan <- recipe_to_plan(r)
    expect_s3_class(plan, "omop_plan_v2")
    skip_if_not_installed("dsOMOP")
    expect_length(.server_semantic_errors(plan), 0L)
  }
})

test_that("an unindexed controls template used as an ordinary roster keeps the age refusal (B3)", {
  for (use in c("output", "parent", "member")) {
    expect_error(recipe_to_plan(.match_recipe(age = TRUE, controls_also = use)),
                 "index-referenced age criterion needs an indexed population", info = use)
  }
  # as the cases of another match it is refused too (it is not indexed)
  expect_error(recipe_to_plan(.match_recipe(age = TRUE, controls_also = "cases")),
               "match cases must be an indexed population|index-referenced age criterion")
  expect_error(recipe_to_plan(omop_recipe(
    populations = list(omop_population("cases", filters = list(omop_filter_sex("F"))),
                       omop_population("controls"),
                       omop_population("m", match = omop_match("cases", "controls",
                                                               omop_event("condition_occurrence", 1)))),
    outputs = omop_output("o", format = "membership", rows = "m"))),
    "match cases must be an indexed population")
  # parity: the server refuses the same plans with the same age rule
  skip_if_not_installed("dsOMOP")
  testthat::local_mocked_bindings(.v2_validate_plan_semantics = function(plan) invisible(TRUE),
                                  .package = "dsOMOPClient")
  for (use in c("output", "parent", "member", "cases")) {
    errs <- .server_semantic_errors(recipe_to_plan(.match_recipe(age = TRUE, controls_also = use)))
    expect_true(any(grepl("index-referenced age criterion needs an indexed population", errs)),
                info = use)
  }
  expect_length(.server_semantic_errors(recipe_to_plan(.match_recipe(age = TRUE))), 0L)
})

# --- B4 -------------------------------------------------------------------------
test_that("OHDSI export refuses a nested exclusion instead of flattening it (B4)", {
  nested <- omop_recipe(concept_sets = list(cs = omop_concept_set(
    c(1, 2), exclude = omop_concept_set(1, exclude = 1))))
  expect_error(recipe_export_concept_sets(nested), "nested exclusion")
  one <- omop_recipe(concept_sets = list(cs = omop_concept_set(c(1, 2), exclude = omop_concept_set(1))))
  back <- recipe_import_concept_sets(omop_recipe(), recipe_export_concept_sets(one))
  expect_identical(as.integer(recipe_concept_sets(back)$cs$exclude), 1L)
})

# --- B6 -------------------------------------------------------------------------
test_that("publish = character(0) is kept from the recipe to the executed plan (B6)", {
  r <- omop_recipe(variables = list(sex = omop_sex()), outputs = omop_output("one", format = "wide"),
                   publish = character(0))
  expect_identical(r$publish, character(0))
  expect_null(dsOMOPClient:::recipe_set_publish(r, NULL)$publish)        # NULL: all outputs
  plan <- recipe_to_plan(r)
  expect_identical(plan$publish, list())
  expect_match(as.character(jsonlite::toJSON(dsOMOPClient:::.v2_plain(plan), auto_unbox = TRUE)),
               '"publish":\\[\\]')
  expect_identical(recipe_import_json(recipe_export_json(r))$publish, character(0))
  expect_identical(recipe_import_yaml(recipe_export_yaml(r))$publish, character(0))
  regenerated <- eval(parse(text = recipe_to_code(r)))
  expect_identical(regenerated$publish, character(0))
  f <- tempfile(fileext = ".json")
  ds.omop.plan.save(plan, f)
  loaded <- ds.omop.plan.load(f)
  expect_identical(loaded$publish, list())
  digest <- dsOMOPClient:::.plan_v2_digest(plan)$sha256
  expect_identical(dsOMOPClient:::.plan_v2_digest(loaded)$sha256, digest)
  expect_identical(dsOMOPClient:::.plan_v2_digest(recipe_to_plan(regenerated))$sha256, digest)
  r$publish <- NULL
  expect_false(identical(dsOMOPClient:::.plan_v2_digest(recipe_to_plan(r))$sha256, digest))
  expect_true("publish.roots" %in% dsOMOPClient:::.plan_v2_operations(plan))
  # execution: zero roots, an ok empty inventory, an empty mapping
  .publication_backend(list(a = .publication_status(character())))
  .with_caps(list(a = .caps()))
  log <- .capture_exec()
  res <- ds.omop.plan.execute(plan)
  expect_identical(res, stats::setNames(character(0), character(0)))
  expect_identical(.decode(log$exec[[3L]])$publish, list())
  # the JSON object {}: what a 2.10.3 server accepts with publish = []
  # (dsOMOP tests/testthat/test-legacy-execution-call.R)
  expect_identical(log$exec[[4L]], "B64:e30")
  expect_error(ds.omop.plan.execute(plan, out = c(one = "X")), "publishes no outputs")
  # a refused status with zero roots is malformed
  .publication_backend(list(a = .publication_status(character(), "refused",
                                                    list(one = .publication_rejection()))))
  .with_caps(list(a = .caps()))
  expect_error(ds.omop.plan.execute(plan), "Invalid plan publication status")
})

test_that("recipe_to_code keeps the list of a single named language-2 variable", {
  r <- omop_recipe(variables = list(sex = omop_sex()), outputs = omop_output("one", format = "wide"))
  code <- recipe_to_code(r)
  expect_match(code, "variables = list(sex = omop_sex())", fixed = TRUE)
  expect_identical(dsOMOPClient:::.plan_v2_digest(recipe_to_plan(eval(parse(text = code))))$sha256,
                   dsOMOPClient:::.plan_v2_digest(recipe_to_plan(r))$sha256)
})

# --- N1 -------------------------------------------------------------------------
test_that("declared events resolve by name in any order; cycles are refused (N1)", {
  forward <- omop_recipe(events = list(omop_cost("rx", name = "cost"),
                                       omop_event("drug_exposure", 1, name = "rx")),
                         outputs = omop_output("o", format = "long", sources = "cost"))
  backward <- forward
  backward$events <- rev(forward$events)
  p1 <- recipe_to_plan(forward)
  p2 <- recipe_to_plan(backward)
  expect_identical(dsOMOPClient:::.plan_v2_digest(p1)$sha256, dsOMOPClient:::.plan_v2_digest(p2)$sha256)
  cycle <- omop_recipe(events = list(omop_cost("b", name = "a"), omop_cost("a", name = "b")),
                       outputs = omop_output("o", format = "long", sources = "a"))
  expect_error(recipe_to_plan(cycle), "cycle: a -> b -> a")
  skip_if_not_installed("dsOMOP")
  expect_length(.server_semantic_errors(p1), 0L)
})

# --- N2 -------------------------------------------------------------------------
test_that("server.version separates old installs from stale profiles and reports recipe_language (N2)", {
  m <- .methods_2102()
  inv <- list(old = list(version = "2.10.2", aggregate = m$aggregate, assign = m$assign),
              stale = list(version = "2.10.2", aggregate = m$aggregate, assign = m$assign),
              new = .current_published_methods(list(new = "N"))$new)
  testthat::local_mocked_bindings(
    .dsomop_published_methods = function(...) inv,
    .ds_safe_aggregate = function(...) list(old = list(version = "2.10.2"),
                                            stale = list(version = "2.10.3"),
                                            new = list(version = "2.10.3")),
    ds.omop.client.version = function() "2.10.3",
    .get_session = function(...) list(conns = list(new = "N"), capabilities = list(new = .caps())),
    .package = "dsOMOPClient")
  got <- ds.omop.server.version(list(old = "O", stale = "S", new = "N"))
  expect_identical(got$ready, c(FALSE, FALSE, TRUE))
  expect_match(got$advice[[1L]], "dsOMOP 2.10.2 is installed and published but predates the publication protocol")
  expect_match(got$advice[[1L]], "Publishing again cannot add these methods.*installs dsOMOP 2.10.3")
  expect_match(got$advice[[2L]], "dsOMOP 2.10.3 is installed but the profile publishes the methods of 2.10.2")
  expect_identical(got$recipe_language[[1L]], "unavailable")
  expect_identical(got$recipe_language[[2L]], "unavailable")
  expect_identical(got$recipe_language[[3L]]$version, 2L)
  expect_true("population.index" %in% unlist(got$recipe_language[[3L]]$operations))
  testthat::local_mocked_bindings(.get_session = function(...) stop("no session"),
                                  .package = "dsOMOPClient")
  expect_identical(ds.omop.server.version(list(old = "O", stale = "S", new = "N"))$recipe_language[[3L]],
                   "not connected")
})
