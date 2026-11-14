# A-31 Q01/Q02: both public importer boundaries use original JSON types.
.audit_expression <- function(id = 201826, ...) {
  list(items = list(c(list(concept = list(CONCEPT_ID = id)), list(...))))
}
.audit_circe <- function(expression) {
  list(ConceptSets = list(list(id = 0L, name = "public", expression = expression)),
       PrimaryCriteria = list(CriteriaList = list(list(ConditionOccurrence = list(CodesetId = 0L))),
                              PrimaryCriteriaLimit = list(Type = "First")))
}
.audit_import_both <- function(expression, pattern) {
  expect_error(recipe_import_concept_sets(omop_recipe(), expression), pattern)
  expect_error(recipe_import_circe(.audit_circe(expression)), pattern)
}

test_that("strict IDs reject lossy coercions in both list import paths", {
  bad <- list(201826.9, -1, NA_real_, NaN, Inf, 2147483648, "201826", TRUE,
              NULL, c(1, 2), list(1), list(x = 1), matrix(1), factor("1"),
              as.Date("2020-01-01"), structure(1, class = "opaque"))
  for (id in bad) .audit_import_both(.audit_expression(id), "invalid_concept_id")
  missing <- list(items = list(list(concept = list(CONCEPT_NAME = "name"))))
  .audit_import_both(missing, "invalid_concept_id")
  for (id in c(0, 201826, 2147483647)) {
    expr <- .audit_expression(id)
    reg <- recipe_import_concept_sets(omop_recipe(), expr)
    expect_equal(reg$concept_sets[[1L]]$concepts, as.integer(id))
    pop <- recipe_import_circe(.audit_circe(expr))
    expect_identical(pop$index_event$concept_id, as.integer(id))
  }
})

test_that("strict JSON numbers and duplicate keys cannot be normalized away", {
  invalid <- c('201826.9', '-1', '2147483648', '"201826"', 'true', 'null', '[1]', '{"x":1}')
  for (id in invalid) {
    json <- paste0('{"items":[{"concept":{"CONCEPT_ID":', id, '}}]}')
    expect_error(recipe_import_concept_sets(omop_recipe(), json), "invalid_concept_id")
    cohort <- paste0('{"ConceptSets":[{"id":0,"expression":', json, '}]}')
    expect_error(recipe_import_circe(cohort), "invalid_concept_id")
  }
  for (item in c('{"concept":{"CONCEPT_ID":1,"CONCEPT_ID":2}}',
                 '{"concept":{"CONCEPT_ID":1},"isExcluded":false,"isExcluded":true}',
                 '{"concept":{"CONCEPT_ID":1},"concept":{"CONCEPT_ID":2}}')) {
    json <- paste0('{"items":[', item, ']}')
    expect_error(recipe_import_concept_sets(omop_recipe(), json), "duplicate_definition_key")
    expect_error(recipe_import_circe(paste0('{"ConceptSets":[{"expression":', json, '}]}')),
                 "duplicate_definition_key")
  }
  reg <- recipe_import_concept_sets(omop_recipe(), '{"items":[{"concept":{"CONCEPT_ID":201826.0}}]}')
  expect_identical(reg$concept_sets[[1L]]$concepts, 201826L)
})

test_that("present flags are Boolean and all items are validated before subset rules", {
  for (flag in c("isExcluded", "includeDescendants", "includeMapped")) {
    for (value in list("false", 0, 1, NULL, logical(0), c(TRUE, FALSE), matrix(TRUE), NA)) {
      expr <- .audit_expression()
      expr$items[[1L]][flag] <- list(value)
      .audit_import_both(expr, "invalid_concept_flag")
    }
  }
  expr <- .audit_expression(includeDescendants = TRUE, includeMapped = TRUE, isExcluded = FALSE)
  pop <- recipe_import_circe(.audit_circe(expr))
  expect_true(pop$index_event$include_descendants)
  expect_true(pop$index_event$include_mapped)
  expr$items[[2L]] <- list(concept = list(CONCEPT_ID = 123), isExcluded = TRUE)
  reg <- recipe_import_concept_sets(omop_recipe(), expr)
  expect_equal(reg$concept_sets[[1L]]$exclude$concepts, 123L)
  expect_identical(reg$concept_sets[[1L]]$exclude$validity_policy, "ohdsi-import-1")
  expect_error(recipe_import_circe(.audit_circe(expr)), "excluded concept-set")
  expr$items[[2L]]$concept$CONCEPT_ID <- "secret-patient-string"
  .audit_import_both(expr, "invalid_concept_id")
  expect_false(grepl("secret-patient-string", tryCatch(recipe_import_circe(.audit_circe(expr)),
                                                      error = conditionMessage)))
  # An unused definition cannot hide malformed public items.
  circe <- .audit_circe(.audit_expression())
  circe$ConceptSets[[2L]] <- list(id = 1, expression = expr)
  expect_error(recipe_import_circe(circe), "invalid_concept_id")
})

test_that("unknown semantics refuse and documented descriptive fields are inert", {
  expr <- .audit_expression()
  expr$items[[1L]]$concept$CONCEPT_NAME <- "public descriptive name"
  expect_s3_class(recipe_import_circe(.audit_circe(expr)), "omop_population")
  expr$items[[1L]]$secret_option <- TRUE
  .audit_import_both(expr, "invalid_concept_item")
  expr <- .audit_expression()
  expr$items[[1L]]$concept$secret_option <- TRUE
  .audit_import_both(expr, "invalid_concept_id")
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  writeLines(jsonlite::toJSON(.audit_circe(.audit_expression()), auto_unbox = TRUE), path)
  expect_s3_class(recipe_import_circe(path), "omop_population")
  expect_error(recipe_import_circe('{"private-string":'), "invalid_definition")
})
