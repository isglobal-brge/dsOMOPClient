# A-24 corrects only frozen corpus row #1. Keep that exact recipe separate
# from the indexed variant; neither the features preset nor root scope is an
# index. These tests establish compilation/selection, not native execution.

.a24_frozen_features <- function() {
  omop_recipe(
    variables = list(
      omop_variable(name = "a", table = "measurement", concept_id = 1,
                    format = "count", time_window = list(start = -90, end = 0)),
      omop_variable(name = "b", table = "measurement", concept_id = 2,
                    format = "binary", time_window = list(start = -90, end = 0))),
    outputs = omop_output(name = "features", type = "features"))
}

.a24_indexed_features <- function() {
  r <- .a24_frozen_features()
  r <- recipe_add_population(r, omop_population(
    "indexed", index_event = omop_index_event(201820, "condition_occurrence")))
  r$outputs$features$population_id <- "indexed"
  r$variables$a$time_window <- list(start = -365L, end = -31L)
  r$variables$b$time_window <- list(start = -30L, end = 0L)
  r
}

test_that("A-24 vendors only the named frozen corpus decision correction", {
  corpus <- utils::read.csv(testthat::test_path("fixtures", "v1-corpus-classes.csv"),
                            stringsAsFactors = FALSE, check.names = FALSE)
  id <- paste0("dsOMOPClient:test-recipe-executable-contract.R:",
               "feature windows compile at episode grain with per-spec scopes")
  exact <- corpus[corpus$plan_id == paste0(id, "#1"), , drop = FALSE]
  adjacent <- corpus[corpus$plan_id == paste0(id, "#2"), , drop = FALSE]
  expect_equal(nrow(exact), 1L)
  expect_identical(exact$class, "unchanged_invalid")
  expect_match(exact$rationale, "A-24", fixed = TRUE)
  expect_match(exact$rationale, "index", fixed = TRUE)
  expect_equal(nrow(adjacent), 1L)
  expect_identical(adjacent$class, "newly_permitted")
  expect_identical(adjacent$rationale, "B-19")
})

test_that("A-24 exact frozen #1 is unindexed and unchanged_invalid", {
  r <- .a24_frozen_features()
  expect_named(r$populations, "base")
  expect_null(r$populations$base$index_event)
  expect_null(r$populations$base$index)
  expect_null(r$populations$base$cohort_definition_id)
  expect_null(r$scope)
  expect_null(r$outputs$features$options$grain)
  before <- serialize(r, NULL)
  refused <- tryCatch(recipe_to_plan(r, version = 1), error = identity)
  expect_s3_class(refused, "dsomop_v1_newly_permitted")
  expect_match(conditionMessage(refused), "grain='episode'", fixed = TRUE)
  expect_error(recipe_to_plan(r, version = 2), "episode grain needs an indexed population")
  expect_error(dsOMOPClient:::.recipe_plan_auto(r),
               "episode grain needs an indexed population.*cannot execute as a v1 plan either")
  expect_identical(serialize(r, NULL), before)
})

test_that("A-24 grain marker is limited to omitted features grain", {
  for (indexed in c(FALSE, TRUE)) {
    for (type in c("features", "wide")) {
      for (grain in list(NULL, "person", "episode")) {
        r <- if (indexed) .a24_indexed_features() else .a24_frozen_features()
        r$outputs$features$type <- type
        r$outputs$features$options$grain <- grain
        result <- tryCatch(recipe_to_plan(r, version = 1), error = identity)
        info <- paste("indexed", indexed, "type", type, "grain", grain %||% "omitted")
        if (identical(grain, "episode")) {
          expect_s3_class(result, "omop_plan")
          expect_false(inherits(result, "omop_plan_v2"), info = info)
          expect_identical(result$outputs$features$representation$grain, "episode", info = info)
          expect_identical(dsOMOPClient:::.recipe_plan_auto(r), result, info = info)
        } else {
          expect_s3_class(result, "error")
          newly <- identical(type, "features") && is.null(grain)
          expect_identical(inherits(result, "dsomop_v1_newly_permitted"), newly, info = info)
          if (!newly) {
            auto <- tryCatch(dsOMOPClient:::.recipe_plan_auto(r), error = identity)
            expect_s3_class(auto, "error")
            expect_false(inherits(auto, "dsomop_v1_newly_permitted"), info = info)
            expect_identical(conditionMessage(auto), conditionMessage(result), info = info)
          }
        }
      }
    }
  }
})

test_that("A-24 indexed variant retains each variable window and passes server semantics", {
  r <- .a24_indexed_features()
  plan <- recipe_to_plan(r, version = 2)
  expect_identical(plan$outputs$features$rows, "indexed")
  expect_identical(plan$outputs$features$grain, "episode")
  expect_identical(plan$variables$a$reducer$window,
                   list(start = -365L, end = -31L, anchor = "index"))
  expect_identical(plan$variables$b$reducer$window,
                   list(start = -30L, end = 0L, anchor = "index"))
  expect_identical(dsOMOPClient:::.recipe_plan_auto(r), plan)
  skip_if_not_installed("dsOMOP")
  ir <- dsOMOP:::.planNormalizeV2(dsOMOPClient:::.v2_plain(plan))
  expect_length(dsOMOP:::.irSemanticErrors(ir), 0L)
  # An independent server check must also reject an unindexed base. Start
  # with accepted native wire and remove the real index, rather than bypass
  # client validation or pretending the frozen fixture had an index.
  unindexed <- dsOMOPClient:::.v2_plain(plan)
  unindexed$populations <- list(base = list(kind = "criteria"))
  unindexed$outputs$features$rows <- "base"
  for (scoped in c(FALSE, TRUE)) {
    if (scoped) unindexed$scope <- list(cohort = 1L)
    errors <- dsOMOP:::.irSemanticErrors(dsOMOP:::.planNormalizeV2(unindexed))
    expect_true(any(grepl("indexed population", errors)), info = paste("root scope", scoped))
  }
})

test_that("A-24 invalid shapes refuse every recipe entry point before transport or assignment", {
  calls <- character()
  reached <- function(...) {
    calls <<- c(calls, "transport")
    stop("Unexpected transport or assignment")
  }
  testthat::local_mocked_bindings(
    .get_session = reached, ds.omop.plan.validate = reached,
    ds.omop.plan.preview = reached, ds.omop.plan.execute = reached,
    .ds_safe_aggregate = reached, .package = "dsOMOPClient")
  testthat::local_mocked_bindings(datashield.assign.expr = reached, .package = "DSI")
  frozen <- .a24_frozen_features()
  scoped <- recipe_set_scope(frozen, cohort = 1L)
  expect_identical(scoped$scope$cohort, 1L)
  expect_identical(scoped$populations, frozen$populations)
  person <- .a24_indexed_features()
  person$outputs$features$options$grain <- "person"
  wide <- .a24_indexed_features()
  wide$outputs$features$type <- "wide"
  cases <- list(frozen = frozen, root_scope = scoped, explicit_person = person, wide = wide)
  for (name in names(cases)) {
    r <- cases[[name]]
    expected <- tryCatch(dsOMOPClient:::.recipe_plan_auto(r), error = identity)
    expect_s3_class(expected, "error")
    for (entry in list(recipe_validate, recipe_preview, recipe_explain, recipe_execute)) {
      err <- tryCatch(entry(r, conns = list(site = "site")), error = identity)
      expect_s3_class(err, "error")
      expect_identical(conditionMessage(err), conditionMessage(expected), info = name)
    }
  }
  # The execution-time cohort argument is also only root scope.
  expect_error(recipe_execute(frozen, cohort = 1L), "episode grain needs an indexed population")
  expect_length(calls, 0L)
})

test_that("A-24 permitted shapes select the same payload at every recipe entry point", {
  .with_caps(list(site = .caps()))
  sent <- list()
  record <- function(name) function(plan, ...) {
    sent[[name]] <<- plan
    invisible(NULL)
  }
  # These endpoint taps measure selection only. Actual native execution is
  # the compiler-gated PostgreSQL regression in test-ws4-adapter-refusals-e2e.R.
  testthat::local_mocked_bindings(
    .dsomop_published_methods = .current_published_methods,
    ds.omop.plan.validate = record("validate"),
    ds.omop.plan.preview = record("preview"),
    ds.omop.plan.execute = record("execute"),
    .ds_safe_aggregate = function(conns, expr, ...) {
      sent$explain <<- .decode(expr[[3L]])
      list(site = list())
    }, .package = "dsOMOPClient")
  for (grain in list(NULL, "episode")) {
    r <- .a24_indexed_features()
    r$outputs$features$options$grain <- grain
    expected <- recipe_to_plan(r, version = if (is.null(grain)) 2L else 1L)
    recipe_validate(r, conns = list(site = "site"))
    recipe_preview(r, conns = list(site = "site"))
    recipe_execute(r, conns = list(site = "site"))
    ex <- recipe_explain(r, conns = list(site = "site"))
    expect_identical(ex$selected, if (is.null(grain)) "v2" else "v1")
    expect_identical(ex$payload, expected)
    expect_identical(sent$validate, expected)
    expect_identical(sent$preview, expected)
    expect_identical(sent$execute, expected)
    encoded <- if (is.null(grain)) dsOMOPClient:::.ds_encode_lossless(expected) else
      dsOMOPClient:::.ds_encode(expected)
    expect_identical(sent$explain, .decode(encoded))
  }
})

test_that("A-24 does not admit unsupported output-level temporal spellings", {
  spellings <- list(
    index_window = list(temporal = list(index_window = list(start = -365L, end = 0L))),
    calendar = list(temporal = list(calendar = list(start = "2020-01-01", end = "2020-12-31"))),
    time_window = list(time_window = list(start = "2020-01-01", end = "2020-12-31")))
  for (name in names(spellings)) {
    r <- .a24_indexed_features()
    r$outputs$features$options <- spellings[[name]]
    diagnostic <- if (identical(name, "time_window")) "calendar time_window has no v2 lowering" else
      "temporal index/calendar windows belong on the variables"
    expect_error(recipe_to_plan(r, version = 2), diagnostic, info = name)
    expect_error(dsOMOPClient:::.recipe_plan_auto(r), diagnostic, info = name)
    expect_error(recipe_validate(r), diagnostic, info = name)
    expect_error(recipe_preview(r), diagnostic, info = name)
    expect_error(recipe_explain(r), diagnostic, info = name)
    expect_error(recipe_execute(r), diagnostic, info = name)
  }
})
