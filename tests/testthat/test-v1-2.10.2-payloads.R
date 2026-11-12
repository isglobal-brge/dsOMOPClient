# WS-8 (review, test quality): v1-selected recipes send exactly the plan the
# tagged dsOMOPClient 2.10.2 compiler produced (fixtures/v1-plans-2.10.2.json,
# generated from tag 2.10.2, client commit 7eb2f6c), and every entry point --
# execute, validate, preview, explain, DP preparation, the sticky query
# library -- sends that full selected payload.

.payload_fixtures <- function() {
  jsonlite::fromJSON(testthat::test_path("fixtures", "v1-plans-2.10.2.json"),
                     simplifyVector = FALSE)
}
.fixture_recipe <- function(f) eval(parse(text = f$recipe), asNamespace("dsOMOPClient"))

test_that("v1-selected recipes encode exactly as the tagged 2.10.2 compiler", {
  fx <- .payload_fixtures()
  expect_length(fx, 16L)
  for (f in fx) {
    plan <- dsOMOPClient:::.recipe_plan_auto(.fixture_recipe(f))
    expect_s3_class(plan, "omop_plan")
    expect_identical(dsOMOPClient:::.ds_encode(plan), f$payload, info = f$name)
  }
})

test_that("execute, DP and sticky send the full selected v1 payload on both protocols", {
  f <- Filter(function(x) identical(x$name, "age_sex_wide"), .payload_fixtures())[[1L]]
  r <- .fixture_recipe(f)
  for (old in list(character(0), "old")) {
    state <- .legacy_federation(old = old, new = "new")
    suppressWarnings(recipe_execute(r))
    for (s in names(state$sent)) expect_identical(state$sent[[s]][[3L]], f$payload, info = s)
    state <- .legacy_federation(old = old, new = "new")
    suppressWarnings(ds.omop.dp.prepare(r))
    for (s in names(state$sent)) {
      expect_identical(as.character(state$sent[[s]][[1L]]), "omopDpPlanExecuteDS")
      expect_identical(state$sent[[s]][[3L]], f$payload, info = s)
    }
  }
  # the sticky query library prepares its input through DP preparation
  age <- Filter(function(x) identical(x$name, "age_sex_wide"), .payload_fixtures())[[1L]]
  state <- .legacy_federation(new = "a")
  spec <- omop_querylibrary_sticky("PE12", variable = "age", breaks = seq(0, 120, 10),
                                   preparation = .fixture_recipe(age))
  dsOMOPClient:::.querylibrary_sticky_prepare(spec, list(a = "a"), "omop")
  expect_identical(as.character(state$sent$a[[1L]]), "omopDpPlanExecuteDS")
  expect_identical(state$sent$a[[3L]], age$payload)
})

test_that("validate, preview and explain send the same payload", {
  f <- Filter(function(x) identical(x$name, "filters"), .payload_fixtures())[[1L]]
  r <- .fixture_recipe(f)
  state <- .legacy_federation(new = "a")
  sent <- list()
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = function(conns, expr, ...) {
      sent[[length(sent) + 1L]] <<- expr
      stats::setNames(rep(list(list()), length(conns)), names(conns))
    }, .package = "dsOMOPClient")
  recipe_validate(r)
  recipe_preview(r)
  recipe_explain(r, conns = list(a = "a"))
  methods <- vapply(sent, function(e) as.character(e[[1L]]), "")
  expect_identical(methods, c("omopPlanPreviewDS", "omopPlanPreviewDS", "omopPlanExplainDS"))
  for (e in sent) expect_identical(e[[3L]], f$payload)
})
