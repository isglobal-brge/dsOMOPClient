.printing_recipe <- function(population) {
  omop_study(population, baseline = list(age = omop_age()), outputs = "baseline")
}

.printing_same_semantics <- function(recipe) {
  before <- recipe
  plan <- recipe_to_plan(recipe)
  text <- paste(capture.output(print(recipe)), collapse = "\n")
  expect_identical(recipe, before)
  expect_identical(recipe_to_plan(recipe), plan)
  expect_identical(dsOMOPClient:::.planNormalizedDigest(recipe_to_plan(recipe))$sha256,
                   dsOMOPClient:::.planNormalizedDigest(plan)$sha256)
  text
}

test_that("new-user printing states actual defaults and generated washout windows", {
  pop <- omop_new_users(1503297)
  before <- pop
  text <- paste(capture.output(print(pop)), collapse = "\n")
  expect_match(text, "washout 365 days", fixed = TRUE)
  expect_match(text, "era grouping ingredient", fixed = TRUE)
  expect_match(text, "era gap 30 days", fixed = TRUE)
  expect_match(text, "selection first_eligible", fixed = TRUE)
  expect_match(text, "observation window [-365, 0]", fixed = TRUE)
  expect_match(text, "exclusion window [-365, -1]", fixed = TRUE)
  expect_identical(pop, before)

  study <- .printing_recipe(pop)
  text <- .printing_same_semantics(study)
  expect_match(text, "washout 365 days", fixed = TRUE)
  expect_match(text, "era grouping ingredient; era gap 30 days", fixed = TRUE)
  expect_match(text, "exclusion window [-365, -1]", fixed = TRUE)
})

test_that("new-user printing reflects explicit choices and zero washout", {
  study <- .printing_recipe(omop_new_users(1503297, washout_days = 14L,
                                           by = "concept", select = "all"))
  text <- .printing_same_semantics(study)
  expect_match(text, "washout 14 days; era grouping concept", fixed = TRUE)
  expect_match(text, "selection all", fixed = TRUE)
  expect_match(text, "observation window [-14, 0]", fixed = TRUE)
  expect_match(text, "exclusion window [-14, -1]", fixed = TRUE)

  zero <- .printing_recipe(omop_new_users(1503297, washout_days = 0L))
  text <- .printing_same_semantics(zero)
  expect_match(text, "washout 0 days", fixed = TRUE)
  expect_match(text, "Washout criteria: none (zero washout)", fixed = TRUE)
  expect_false(grepl("exclusion window", text, fixed = TRUE))
})

test_that("inherited new-user choices survive code JSON YAML and parameter binding", {
  template <- .printing_recipe(omop_new_users(
    1503297, washout_days = omop_param("washout", "integer", default = 0L)))
  expect_output(print(template), "Washout criteria deferred")
  for (days in c(0L, 21L)) {
    study <- recipe_bind(template, washout = days)
    env <- new.env(parent = asNamespace("dsOMOPClient"))
    eval(parse(text = recipe_to_code(study)), envir = env)
    variants <- list(study, env$recipe,
                     recipe_import_json(recipe_export_json(study)),
                     recipe_import_yaml(recipe_export_yaml(study)))
    for (variant in variants) {
      text <- .printing_same_semantics(variant)
      expect_match(text, paste0("washout ", days, " days"), fixed = TRUE)
      expect_match(text, "era grouping ingredient; era gap 30 days", fixed = TRUE)
      expect_identical(recipe_to_plan(variant), recipe_to_plan(study))
    }
  }
})

test_that("zero-washout studies named base retain their explicit index in advanced code", {
  study <- omop_study(omop_new_users(1503297, washout_days = 0L),
                      baseline = list(age = omop_age()), outputs = "baseline", name = "base")
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(study)), envir = env)
  expect_identical(env$recipe$populations$base, study$populations$base)
  expect_identical(recipe_to_plan(env$recipe), recipe_to_plan(study))
  text <- .printing_same_semantics(env$recipe)
  expect_match(text, "washout 0 days", fixed = TRUE)
})
