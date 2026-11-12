# WS-2: the role-registry tables vendored from dsOMOP (inst/ohdsi/roles-5.3.csv,
# roles-5.4.csv). The language 2.0 normal form is the frozen WS-0 artefact
# R/plan_normalize_v2.R (amendment A-11), tested by test-ws0-normalize.R; the
# server's .irNormalize() calls the same function, and its tests compare the
# two packages' normal forms and digests.

test_that("index-event tables follow the vendored role registry (B-20)", {
  tables <- .role_event_tables()
  expect_true(all(c("condition_occurrence", "drug_exposure", "death", "visit_detail",
                    "observation_period", "episode") %in% tables))
  expect_false(any(c("cost", "episode_event", "person", "concept") %in% tables))
  expect_identical(omop_index_event(NULL, "death")$table, "death")
  expect_error(omop_index_event(NULL, "cost"), "must be one of")
})
