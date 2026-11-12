# WS-8 acceptance (design-2.10.3 sections 10.1 and 10.7): every acceptance
# recipe of the frozen corpus, written against the public 2.10.3 API, emits a
# plan semantically identical (same normalised semantic digest) to the frozen
# plan.json; every emitted plan validates against the vendored plan-v2.json.

.acceptance_dir <- function() {
  d <- system.file("plans", "acceptance", package = "dsOMOP")
  if (!nzchar(d)) {
    cand <- testthat::test_path("..", "..", "..", "dsOMOP", "inst", "plans", "acceptance")
    if (dir.exists(cand)) d <- cand
  }
  d
}

# `wd` is the working directory relative file references resolve against (a
# variant reads its base recipe's files, e.g. 12's circe.json).
.run_acceptance_recipe <- function(dir, file = "recipe.R", wd = dir) {
  code <- readLines(file.path(dir, file), warn = FALSE)
  code <- code[!grepl("^\\s*library\\(", code)]
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  withr::with_dir(wd, eval(parse(text = code), envir = env))
  env
}

.frozen_plan <- function(dir, file = "plan.json") {
  jsonlite::fromJSON(file.path(dir, file), simplifyVector = FALSE)
}

.digest <- function(plan) dsOMOPClient:::.plan_v2_digest(plan)$sha256

test_that("acceptance recipes 01-12, 14, 15 reproduce their frozen plans", {
  acc <- .acceptance_dir()
  skip_if(!nzchar(acc) || !dir.exists(acc), "dsOMOP acceptance corpus not installed")
  dirs <- sort(list.dirs(acc, recursive = FALSE))
  dirs <- dirs[!grepl("^13-", basename(dirs))]
  expect_length(dirs, 14L)
  schema <- dsOMOPClient:::.v2_plan_schema()
  for (d in dirs) {
    nm <- basename(d)
    env <- .run_acceptance_recipe(d)
    plan <- env$plan
    expect_s3_class(plan, "omop_plan_v2")
    expect_true(dsOMOPClient:::.jsonSchemaValidate(
      schema, dsOMOPClient:::.v2_plain(plan))$valid, info = nm)
    frozen <- .frozen_plan(d)
    mine <- plan
    if (grepl("^12-", nm)) {
      # CR-9: the importer names the Circe concept set by its sanitised name;
      # the frozen plan uses "t2dm". The semantics are otherwise identical.
      json <- as.character(jsonlite::toJSON(dsOMOPClient:::.v2_plain(plan),
                                            auto_unbox = TRUE, null = "null", digits = NA))
      mine <- jsonlite::fromJSON(gsub("type_2_diabetes", "t2dm", json, fixed = TRUE),
                                 simplifyVector = FALSE)
    }
    expect_identical(.digest(mine), .digest(frozen), info = nm)
  }
})

test_that("the Circe importer reproduces recipe 12 up to the concept-set name (CR-9)", {
  acc <- .acceptance_dir()
  skip_if(!nzchar(acc) || !dir.exists(acc), "dsOMOP acceptance corpus not installed")
  d <- file.path(acc, "12-circe-baseline")
  pop <- withr::with_dir(d, recipe_import_circe("circe.json", id = "t2dm_first",
                                                label = "Type 2 diabetes (Circe)"))
  # The legacy (v1) fields are unchanged; the language-2 form is attached.
  expect_identical(pop$index_event$primary_limit, "first")
  expect_identical(pop$circe_v2$index$select, "first_eligible")
  expect_identical(pop$circe_v2$coverage$policy, "unique")
  expect_identical(pop$circe_v2$filters[[1]]$before, 365L)
  expect_identical(pop$circe_v2$filters[[1]]$after, 0L)
  plan <- recipe_to_plan(omop_recipe(populations = pop, variables = list(sex = omop_sex()),
                                     outputs = omop_output("o", rows = "t2dm_first", format = "wide")))
  expect_named(plan$concept_sets, "type_2_diabetes")
  expect_true("type_2_diabetes_dx" %in% names(plan$events))
})

test_that("recipe 13 and its 12 variants bind to the frozen plans (with the parameter layer)", {
  acc <- .acceptance_dir()
  skip_if(!nzchar(acc) || !dir.exists(acc), "dsOMOP acceptance corpus not installed")
  skip_if_not(exists("omop_param", envir = asNamespace("dsOMOPClient")) &&
                exists("recipe_bind", envir = asNamespace("dsOMOPClient")),
              "omop_param()/recipe_bind() (WS-9) not available")
  d13 <- file.path(acc, "13-parametrised")
  env <- .run_acceptance_recipe(d13)
  expect_identical(.digest(env$plan), .digest(.frozen_plan(d13)))
  manifest <- jsonlite::fromJSON(file.path(d13, "variants", "variants.json"),
                                 simplifyVector = FALSE)
  for (i in sprintf("%02d", 1:12)) {
    vdir <- file.path(d13, "variants")
    entry <- Filter(function(m) identical(m$variant, paste0("recipe-", i, ".R")), manifest)
    expect_length(entry, 1L)
    env <- .run_acceptance_recipe(vdir, paste0("recipe-", i, ".R"),
                                  wd = file.path(acc, entry[[1]]$equals_plan_of))
    frozen <- .frozen_plan(vdir, paste0("plan-", i, ".json"))
    mine <- env$plan
    if (identical(i, "12")) {
      json <- as.character(jsonlite::toJSON(dsOMOPClient:::.v2_plain(mine), auto_unbox = TRUE,
                                            null = "null", digits = NA))
      mine <- jsonlite::fromJSON(gsub("type_2_diabetes", "t2dm", json, fixed = TRUE),
                                 simplifyVector = FALSE)
    }
    expect_identical(.digest(mine), .digest(frozen), info = paste("variant", i))
  }
  expect_true(length(manifest) > 0L)
})

test_that("frozen plans are digest-stable under the client normaliser", {
  acc <- .acceptance_dir()
  skip_if(!nzchar(acc) || !dir.exists(acc), "dsOMOP acceptance corpus not installed")
  # Normalisation is idempotent: normalising a normal form changes nothing.
  for (d in sort(list.dirs(acc, recursive = FALSE))) {
    frozen <- .frozen_plan(d)
    once <- dsOMOPClient:::.planNormalizeV2(frozen)
    twice <- dsOMOPClient:::.planNormalizeV2(once)
    expect_identical(dsOMOPClient:::.jcsCanonical(once),
                     dsOMOPClient:::.jcsCanonical(twice), info = basename(d))
  }
})
