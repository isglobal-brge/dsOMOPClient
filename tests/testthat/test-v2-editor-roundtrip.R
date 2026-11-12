.editor_block <- function() {
  block <- omop_variable_block(
    id = "dx", table = "condition_occurrence",
    concept_ids = c(201826, 320128), concept_names = c("t2dm", "htn"),
    format = "binary", expand = TRUE,
    where = omop_where(condition_type_concept_id == 32817))
  block$block_version <- "1.2"
  block
}

.editor_roundtrip <- function(recipe) {
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(recipe)), envir = env)
  rebuilt <- env$recipe
  expect_identical(names(rebuilt$variables), names(recipe$variables))
  expect_identical(rebuilt$variables, recipe$variables)
  expect_identical(rebuilt$blocks, recipe$blocks)
  original_plan <- recipe_to_plan(recipe)
  rebuilt_plan <- recipe_to_plan(rebuilt)
  expect_identical(rebuilt_plan, original_plan)
  expect_identical(dsOMOPClient:::.planNormalizedDigest(rebuilt_plan)$sha256,
                   dsOMOPClient:::.planNormalizedDigest(original_plan)$sha256)
  invisible(rebuilt)
}

test_that("removed block variables stay removed through advanced code and serialization", {
  original <- omop_recipe(blocks = .editor_block(),
                          outputs = omop_output("w", type = "wide"))
  edited <- recipe_remove_variable(original, "htn")
  expect_identical(names(edited$variables), "t2dm")
  .editor_roundtrip(edited)
  .editor_roundtrip(recipe_import_json(recipe_export_json(edited)))
  .editor_roundtrip(recipe_import_yaml(recipe_export_yaml(edited)))

  edited <- recipe_add_variable(edited, omop_variable_age())
  edited <- recipe_remove_variable(edited, "t2dm")
  expect_identical(names(edited$variables), "age")
  .editor_roundtrip(edited)
  expect_identical(names(original$variables), c("t2dm", "htn"))
})

test_that("edited block members retain their definitions order and provenance", {
  original <- omop_recipe(blocks = .editor_block(),
                          outputs = omop_output("w", type = "wide"))
  replacement <- original$variables$t2dm
  replacement$format <- "count"
  replacement$time_window <- list(start = -30L, end = 0L)
  edited <- recipe_remove_variable(original, "t2dm")
  edited <- recipe_add_variable(edited, replacement)
  expect_identical(names(edited$variables), c("htn", "t2dm"))
  rebuilt <- .editor_roundtrip(edited)
  expect_identical(rebuilt$variables$t2dm$block_id, "dx")
  expect_identical(rebuilt$variables$t2dm$block_version, "1.2")
  expect_true(rebuilt$variables$t2dm$expand)
  .editor_roundtrip(recipe_import_json(recipe_export_json(edited)))
})

test_that("block expansion collisions preserve standalone and generated names", {
  original <- omop_recipe(
    variables = omop_variable(name = "t2dm", table = "condition_occurrence",
                             concept_id = 4329847, format = "binary"),
    outputs = omop_output("w", type = "wide"))
  original <- recipe_add_block(original, .editor_block())
  expect_identical(names(original$variables), c("t2dm", "t2dm_2", "htn"))
  .editor_roundtrip(original)
  .editor_roundtrip(recipe_remove_variable(original, "t2dm_2"))
})

test_that("conflicting block IDs refuse atomically and identical re-additions are idempotent", {
  first <- .editor_block()
  original <- omop_recipe(blocks = first, outputs = omop_output("w", type = "wide"))
  second <- omop_variable_block(id = "dx", table = "condition_occurrence",
                                concept_ids = 4329847, concept_names = "mi", format = "binary")
  before <- original
  expect_error(recipe_add_block(original, second), "Block ID 'dx'.*conflicting definition")
  expect_identical(original, before)
  expect_error(omop_recipe(blocks = list(first, second)), "Block ID 'dx'.*conflicting definition")
  expect_identical(recipe_add_block(original, first), original)
  edited <- recipe_remove_variable(original, "htn")
  expect_identical(recipe_add_block(edited, first), edited)
  .editor_roundtrip(edited)
})

test_that("prefixed merged block edits preserve advanced plans digests and metadata", {
  original <- omop_recipe(blocks = .editor_block(),
                          outputs = omop_output("w", type = "wide"))
  merged <- recipe_merge(original, original, prefix = "b")
  edited <- recipe_remove_variable(merged, "b_htn")
  edited <- recipe_add_output(edited, omop_output("b_w", type = "wide", variables = "b_t2dm"))
  replacement <- edited$variables$b_t2dm
  replacement$time_window <- list(start = -180L, end = -1L)
  edited <- recipe_add_variable(recipe_remove_variable(edited, "b_t2dm"), replacement)
  rebuilt <- .editor_roundtrip(edited)
  expect_identical(rebuilt$blocks$b_dx$name_prefix, "b")
  expect_identical(rebuilt$variables$b_t2dm$block_id, "b_dx")
  expect_identical(rebuilt$variables$b_t2dm$block_version, "1.2")
  expect_false("b_htn" %in% names(rebuilt$variables))
  .editor_roundtrip(recipe_import_json(recipe_export_json(edited)))
  .editor_roundtrip(recipe_import_yaml(recipe_export_yaml(edited)))
})
