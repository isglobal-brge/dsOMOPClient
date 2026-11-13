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
  # The real sticky preparation path also retains the tagged payload and
  # selects one protocol for every site in an old/new federation.
  for (old in list(character(0), "old")) {
    state <- .legacy_federation(old = old, new = "new")
    spec <- omop_querylibrary_sticky("PE12", variable = "age", breaks = seq(0, 120, 10),
                                     preparation = r)
    sites <- c(old, "new")
    conns <- stats::setNames(as.list(sites), sites)
    suppressWarnings(dsOMOPClient:::.querylibrary_sticky_prepare(spec, conns, "omop"))
    expect_identical(unname(state$assigns), rep(1L, length(sites)))
    for (s in sites) {
      expect_identical(as.character(state$sent[[s]][[1L]]), "omopDpPlanExecuteDS")
      expect_identical(state$sent[[s]][[3L]], f$payload)
      if (length(old)) expect_null(as.list(state$sent[[s]])$commit_policy)
      else expect_identical(as.list(state$sent[[s]])$commit_policy, "server")
    }
    if (length(old)) expect_false(any(c("omopPlanStatusDS", "omopStagedRevokeDS") %in% .called(state)))
  }
})

test_that("validate, preview and explain preserve every tagged representative payload", {
  state <- .legacy_federation(old = "old", new = "new")
  sent <- list()
  destinations <- list()
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = function(conns, expr, ...) {
      sent[[length(sent) + 1L]] <<- expr
      destinations[[length(destinations) + 1L]] <<- names(conns)
      stats::setNames(rep(list(list()), length(conns)), names(conns))
    }, .package = "dsOMOPClient")
  for (f in .payload_fixtures()) {
    sent <- destinations <- list()
    r <- .fixture_recipe(f)
    recipe_validate(r)
    recipe_preview(r)
    ex <- recipe_explain(r, conns = list(old = "old", new = "new"))
    methods <- vapply(sent, function(e) as.character(e[[1L]]), "")
    expect_identical(methods, c("omopPlanPreviewDS", "omopPlanPreviewDS", "omopPlanExplainDS"), info = f$name)
    for (e in sent) expect_identical(e[[3L]], f$payload, info = f$name)
    expect_identical(destinations, list(c("old", "new"), c("old", "new"), "new"), info = f$name)
    expect_identical(dsOMOPClient:::.ds_encode(ex$payload), f$payload, info = f$name)
    expect_null(ex$plan)
    expect_null(ex$digest)
    expect_match(paste(ex$text, collapse = "\n"), "Server explain is unavailable on old")
    expect_match(paste(ex$text, collapse = "\n"), "Execution would run the legacy call")
  }
})

test_that("sticky native preparation refuses an old/new federation before every assignment", {
  state <- .legacy_federation(old = "old", new = "new")
  native <- omop_recipe(variables = list(age = omop_age()),
                        outputs = omop_output("w", format = "wide"))
  spec <- omop_querylibrary_sticky("PE12", variable = "age", breaks = seq(0, 120, 10),
                                   preparation = native)
  expect_error(dsOMOPClient:::.querylibrary_sticky_prepare(spec,
    list(old = "old", new = "new"), "omop"), "old.*2.10.3|old.*Upgrade")
  expect_identical(unname(state$assigns), c(0L, 0L))
  expect_length(state$sent, 0L)
  expect_false(any(c("omopPlanStatusDS", "omopStagedRevokeDS") %in% .called(state)))
})
