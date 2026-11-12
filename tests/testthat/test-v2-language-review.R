# Additional WS-8 language regressions: exercise the shared A-11 population
# rule and declarative adapter dependencies against the real server validator.

.language_server_errors <- function(plan) {
  dsOMOP:::.irSemanticErrors(dsOMOP:::.planNormalizeV2(dsOMOPClient:::.v2_plain(plan)))
}

.language_setop_match <- function(op, indexed_first = TRUE, age = FALSE) {
  members <- if (indexed_first) c("indexed", "plain") else c("plain", "indexed")
  selected <- do.call(omop_population, c(list(id = "selected"),
                                        stats::setNames(list(members), op)))
  omop_recipe(
    events = list(omop_event("condition_occurrence", 1, name = "mi")),
    populations = list(
      omop_population("indexed", index = omop_index("mi")),
      omop_population("plain"), selected,
      omop_population("cases", parent_id = "selected",
                      filters = if (age) list(omop_age_is(min = 18)) else list()),
      omop_population("controls", filters = list(omop_age_is(min = 18))),
      omop_population("matched", match = omop_match("cases", "controls", "mi"))),
    outputs = omop_output("members", rows = "matched", format = "membership"))
}

test_that("matching and inherited age use A-11 set-operation indexedness (B3)", {
  skip_if_not_installed("dsOMOP")
  for (op in c("union", "intersect", "setdiff")) {
    orders <- if (op == "union") c(TRUE, FALSE) else TRUE
    for (indexed_first in orders) {
      plan <- recipe_to_plan(.language_setop_match(op, indexed_first, age = TRUE))
      expect_identical(plan$outputs$members$grain, "episode", info = op)
      expect_identical(length(.language_server_errors(plan)), 0L, info = op)
    }
  }
})

test_that("right-only indexes do not make intersect or setdiff cases indexed (B3)", {
  skip_if_not_installed("dsOMOP")
  for (op in c("intersect", "setdiff")) {
    r <- .language_setop_match(op, indexed_first = FALSE)
    expect_error(recipe_to_plan(r), "match cases must be an indexed population", info = op)
  }
  # Bypass only the client check to prove the independent server applies the
  # same rule; the ordinary positive plans above use no client override.
  testthat::local_mocked_bindings(.v2_validate_plan_semantics = function(plan) invisible(TRUE),
                                  .package = "dsOMOPClient")
  for (op in c("intersect", "setdiff")) {
    errors <- .language_server_errors(recipe_to_plan(.language_setop_match(op, FALSE)))
    expect_true(any(grepl("cases population must be indexed", errors)), info = op)
  }
})

test_that("forward episode adapters and interval dependencies preserve meaning (N1)", {
  skip_if_not_installed("dsOMOP")
  events <- list(
    omop_episode_members("episode", tables = "drug_exposure", name = "members"),
    omop_cost("rx", currency = 44818668, name = "costs"),
    omop_event("episode", 32528, name = "episode"),
    omop_event("drug_exposure", 1, name = "rx"))
  make <- function(events) omop_recipe(
    events = events,
    intervals = list(omop_era("rx", name = "treatment")),
    outputs = list(omop_output("linked", format = "long", sources = "members"),
                   omop_output("costs", format = "long", sources = "costs"),
                   omop_output("courses", format = "long", sources = "treatment")))
  forward <- recipe_to_plan(make(events))
  backward <- recipe_to_plan(make(rev(events)))
  expect_identical(dsOMOPClient:::.plan_v2_digest(forward)$sha256,
                   dsOMOPClient:::.plan_v2_digest(backward)$sha256)
  expect_identical(forward$events$members$episode_members$episode, list(event = "episode"))
  expect_identical(forward$events$costs$cost$event, list(event = "rx"))
  expect_identical(forward$intervals$treatment$source, list(event = "rx"))
  expect_length(.language_server_errors(forward), 0L)
  expect_length(.language_server_errors(backward), 0L)
})

test_that("named dependency failures distinguish cycles and missing declarations (N1)", {
  make <- function(events) omop_recipe(events = events,
                                      outputs = omop_output("o", format = "long", sources = "a"))
  expect_error(recipe_to_plan(make(list(omop_cost("a", name = "a")))), "cycle: a -> a")
  expect_error(recipe_to_plan(make(list(omop_cost("b", name = "a"), omop_cost("c", name = "b"),
                                        omop_cost("a", name = "c")))), "cycle: a -> b -> c -> a")
  expect_error(recipe_to_plan(make(list(omop_cost("missing", name = "a")))),
               "references event 'missing'.*does not declare")
  # A failed traversal must not leave state behind for the next compilation.
  good <- make(list(omop_cost("rx", name = "a"), omop_event("drug_exposure", 1, name = "rx")))
  expect_s3_class(recipe_to_plan(good), "omop_plan_v2")
})

test_that("both features grain refusal sites retain the A-14 boundary (B5)", {
  variable <- omop_variable(name = "n", table = "measurement", concept_id = 1,
                            format = "count")
  # The first branch handles a windowed features preset before constructing
  # its v1 feature plan; the later branch also handles a wide preset with a
  # feature representation. An explicit person grain stays an ordinary error.
  for (type in c("features", "wide")) {
    for (grain in list(NULL, "person", "episode")) {
      options <- list(temporal = list(index_window = list(start = -90L, end = 0L)))
      if (!is.null(grain)) options$grain <- grain
      r <- omop_recipe(variables = list(variable), outputs = omop_output("f", type = type,
                                                                         options = options))
      result <- tryCatch(recipe_to_plan(r, version = 1), error = identity)
      # Exercise the separate decoration guard directly as well; the normal
      # builder can reject an incompatible grain before reaching this guard.
      undecorated <- list(type = "event_level", table = "measurement",
                           representation = list(format = "features"))
      decorated <- tryCatch(dsOMOPClient:::.decorate_recipe_plan_output(
        undecorated, r$outputs$f, r$variables, "f"), error = identity)
      if (identical(grain, "episode")) {
        expect_s3_class(result, "omop_plan")
        expect_identical(decorated$representation$grain, "episode")
      } else {
        expect_s3_class(result, "error")
        expect_s3_class(decorated, "error")
        expect_identical(inherits(result, "dsomop_v1_newly_permitted"),
                         identical(type, "features") && is.null(grain))
        expect_identical(inherits(decorated, "dsomop_v1_newly_permitted"),
                         identical(type, "features") && is.null(grain))
      }
    }
  }
})
