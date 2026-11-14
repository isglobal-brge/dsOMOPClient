# Maintainer-only artifact writer. Run on the pod with the matching sibling
# installed; review diffs before accepting a new immutable template version.
# Never invoked by package code or by normal tests.
pkgload::load_all(quiet = TRUE)
source("tests/testthat/helper-recipe-library.R")
write_json <- function(x, file) writeLines(as.character(jsonlite::toJSON(x,
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)), file)
for (id in recipe_library_list()$id) {
  cat(id, "\n")
  dir <- file.path("tests/testthat/fixtures/recipe-library", id, "1.0.0/default")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  r <- .library_test_recipe(id, TRUE)
  p <- recipe_to_plan(r, version = 2L)
  digest <- .plan_v2_digest(p)
  write_json(lapply(.library_test_bindings(id, TRUE), .recipe_strip_classes), file.path(dir, "bindings.json"))
  writeLines(recipe_to_code(r), file.path(dir, "recipe.R"))
  write_json(.v2_plain(p), file.path(dir, "plan.json"))
  write_json(.planNormalizeV2(p), file.path(dir, "normalized.json"))
  writeLines(digest$payload, file.path(dir, "semantic.json"))
  writeLines(digest$sha256, file.path(dir, "semantic.sha256"))
  write_json(list(id = id, template_version = "1.0.0",
    definition_checksum = r$meta$recipe_library[[1]]$definition_checksum,
    client_version = as.character(packageVersion("dsOMOPClient")),
    server_version = as.character(packageVersion("dsOMOP")),
    normalizer = "A-11", fixture = "library-synthetic-v1", vocabulary = "library-synthetic-v1",
    operations = .plan_v2_operations(p)), file.path(dir, "manifest.json"))
}
