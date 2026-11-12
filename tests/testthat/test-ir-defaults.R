# WS-2: the shared language 2.0 normaliser (R/ir_defaults.R, vendored
# byte-identically from dsOMOP) and the vendored role-registry tables.

.ird_example <- function(name) {
  jsonlite::fromJSON(system.file("schema", "examples", name, package = "dsOMOPClient"),
                     simplifyVector = FALSE)
}
.ird_schema <- function() .jsonSchemaPackaged("plan-v2", package = "dsOMOPClient")

test_that("the client normaliser expands the documented defaults", {
  b <- .irNormalizeWire(.ird_example("plan-v2-example-B.json"), .ird_schema())
  expect_identical(b$populations$cases$index$n, 1L)
  expect_identical(b$populations$cases$coverage$policy, "unique")
  expect_identical(b$outputs$cc_table$grain, "episode")
  expect_identical(unlist(b$publish), "cc_table")
  expect_identical(b$scope$combine, "union")
  expect_false(grepl("legacy_", .jcsCanonical(b), fixed = TRUE))
  # fixed point, and still a valid wire plan
  again <- .irNormalizeWire(b, .ird_schema())
  expect_identical(.jcsCanonical(again), .jcsCanonical(b))
  expect_true(.jsonSchemaValidate(.ird_schema(), b)$valid)
})

test_that("normalised digests are independent of default spelling", {
  a <- .ird_example("plan-v2-example-A.json")
  n1 <- .irNormalizeWire(a, .ird_schema())
  n2 <- .irNormalizeWire(n1, .ird_schema())
  expect_identical(.planSemanticDigest(n1)$sha256, .planSemanticDigest(n2)$sha256)
})

test_that("index-event tables follow the vendored role registry (B-20)", {
  tables <- .role_event_tables()
  expect_true(all(c("condition_occurrence", "drug_exposure", "death", "visit_detail",
                    "observation_period", "episode") %in% tables))
  expect_false(any(c("cost", "episode_event", "person", "concept") %in% tables))
  expect_identical(omop_index_event(NULL, "death")$table, "death")
  expect_error(omop_index_event(NULL, "cost"), "must be one of")
})
