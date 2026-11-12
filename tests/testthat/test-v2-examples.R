# WS-8 (design-2.10.3 sections 2.12, 4.2-4.4, 10.1): the canonical examples A
# and B equal the plans emitted from their R recipes after both sides are
# normalised; the semantic digest behaves as section 4.4 fixes it.

.example <- function(name) {
  d <- system.file("schema", "examples", package = "dsOMOPClient")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "schema", "examples")
  jsonlite::fromJSON(file.path(d, paste0("plan-v2-example-", name, ".json")),
                     simplifyVector = FALSE)
}

.example_a_recipe <- function() {
  # The section 2.12 worked example, verbatim.
  t2dm  <- omop_concept_set(201826, descendants = TRUE, name = "t2dm")
  metf  <- omop_concept_set(1503297, descendants = TRUE, name = "metformin")
  mi    <- omop_concept_set(4329847, descendants = TRUE, name = "mi")
  hba1c <- omop_event("measurement", 3004410,
                      unit = omop_unit(8554, convert = list("8753" = c(factor = 0.0915, offset = 2.15))),
                      where = omop_where(!is.na(value)), name = "hba1c")
  metf_rx  <- omop_event("drug_exposure", metf, end = list(fallback = "days_supply"), name = "metf_rx")
  metf_era <- omop_era(metf_rx, gap = 30L, name = "metf_era")
  mi_ev    <- omop_event("condition_occurrence", mi, name = "mi")
  omop_recipe(
    events = list(hba1c, metf_rx, mi_ev),
    intervals = list(metf_era),
    populations = list(omop_population("new_users",
      index = omop_index(metf_era, select = "first_eligible", end = omop_end("interval_end")),
      filters = list(
        omop_observed(before = 365),
        omop_has_not(metf_rx, window = omop_window(-365, -1)),
        omop_has(omop_event("condition_occurrence", t2dm), window = omop_window(NULL, 0))))),
    variables = list(
      omop_age(), omop_sex(),
      omop_variable(name = "hba1c_base", event = hba1c,
        reducer = omop_reduce("closest_value", window = omop_window(-180, 0), side = "before"))),
    outputs = list(
      omop_output("baseline", rows = "new_users", format = "wide"),
      omop_output("tte", rows = "new_users", format = "survival",
        options = list(outcomes = list(mi = "mi"), format = "survival",
          tars = list(itt = omop_window(1, NULL),
                      on_treatment = omop_window(omop_bound("index", 1), omop_bound("index_end", 30))),
          censor = list("observation_end", "death"),
          prior_outcome = list(lookback = 365, action = "exclude")))))
}

.example_b_recipe <- function() {
  mi <- omop_event("condition_occurrence",
                   omop_concept_set(4329847, descendants = TRUE, name = "mi"), name = "mi")
  statin_rx <- omop_event("drug_exposure",
                          omop_concept_set(c(1539403, 1545958, 1551860), descendants = TRUE,
                                           name = "statins"),
                          end = list(fallback = "days_supply"), name = "statin_rx")
  diabetes <- omop_event("condition_occurrence", 201826, descendants = TRUE, name = "diabetes")
  omop_recipe(
    events = list(mi, statin_rx, diabetes),
    populations = list(
      omop_population("cases", label = "First MI",
                      index = omop_index(mi, select = "first_eligible",
                                         end = omop_end("fixed", days = 0)),
                      filters = list(omop_observed(before = 365))),
      omop_population("eligible", label = "Observed one year",
                      filters = list(omop_observed(before = 365))),
      omop_population("cc", label = "Nested case-control", match = omop_match(
        cases = "cases", controls = "eligible", outcome = mi,
        match = list("sex", omop_age_band(5), "calendar_year",
                     omop_strata(diabetes, window = omop_window(NULL, -1))),
        controls_per_case = 4, seed = 20261111))),
    variables = list(
      set_id = omop_population_column("set_id"), is_case = omop_population_column("is_case"),
      w = omop_population_column("sampling_weight"), age = omop_age(), sex = omop_sex(),
      statin_365 = omop_variable(name = "statin_365", event = statin_rx,
                                 reducer = omop_reduce("binary", window = omop_window(-365, -1))),
      statin_days = omop_variable(name = "statin_days", event = statin_rx,
                                  reducer = omop_reduce("covered_days", window = omop_window(-365, -1)))),
    outputs = omop_output("cc_table", rows = "cc", format = "wide", grain = "episode",
                          columns = c("set_id", "is_case", "w", "age", "sex", "statin_365",
                                      "statin_days"),
                          options = list(date_handling = "relative"), result_symbol = "D_cc"))
}

test_that("canonical example A equals the section 2.12 recipe after normalisation", {
  plan <- recipe_to_plan(.example_a_recipe())
  a <- dsOMOPClient:::.planNormalizeV2(plan)
  b <- dsOMOPClient:::.planNormalizeV2(.example("A"))
  expect_identical(dsOMOPClient:::.planSemanticDigest(a)$payload,
                   dsOMOPClient:::.planSemanticDigest(b)$payload)
  # deterministic interning: the inline t2dm event is named t2dm_dx
  expect_true("t2dm_dx" %in% names(plan$events))
  expect_identical(plan$events$t2dm_dx$concept_set, "t2dm")
})

test_that("canonical example B equals the matching recipe after normalisation", {
  plan <- recipe_to_plan(.example_b_recipe())
  expect_identical(dsOMOPClient:::.plan_v2_digest(plan)$sha256,
                   dsOMOPClient:::.plan_v2_digest(.example("B"))$sha256)
})

test_that("the semantic digest binds versions and ignores advisory fields", {
  base <- dsOMOPClient:::.planNormalizeV2(.example("A"))
  d0 <- dsOMOPClient:::.planSemanticDigest(base)$sha256
  sv <- base; sv$meta$semantic_version <- "2.1.0"
  expect_false(identical(dsOMOPClient:::.planSemanticDigest(sv)$sha256, d0))
  rs <- base; rs$outputs$baseline$result_symbol <- "D_other"
  expect_identical(dsOMOPClient:::.planSemanticDigest(rs)$sha256, d0)
  lb <- base; lb$populations$new_users$label <- "Another label"
  expect_identical(dsOMOPClient:::.planSemanticDigest(lb)$sha256, d0)
  cv <- base; cv$meta$client_version <- "9.9.9"
  expect_identical(dsOMOPClient:::.planSemanticDigest(cv)$sha256, d0)
  # a clinical change does change it
  cl <- base; cl$intervals$metf_era$gap <- 31L
  expect_false(identical(dsOMOPClient:::.planSemanticDigest(cl)$sha256, d0))
})

test_that("normalisation expands defaults exactly once and keeps explicit values", {
  plan <- recipe_to_plan(.example_a_recipe())
  n <- dsOMOPClient:::.planNormalizeV2(plan)
  expect_identical(n$populations$new_users$coverage,
                   list(policy = "unique", on_ambiguous = "drop", on_missing = "drop", gap = 0L))
  expect_identical(unlist(n$publish), c("baseline", "tte"))
  expect_identical(unlist(n$outputs$baseline$columns), c("age", "sex", "hba1c_base"))
  expect_identical(n$outputs$tte$options$event_order, "first")
  expect_identical(n$outputs$tte$options$tie_policy, "priority")
  expect_identical(n$events$hba1c$on_missing_table, "error")
  expect_null(n$events$hba1c$visit)
  expect_true("visit" %in% names(n$events$hba1c))
  expect_identical(n$variables$hba1c_base$reducer$missing, "skip")
  expect_identical(n$options$translate_concepts, TRUE)
  # the emitted plan validates; the normal form does too
  schema <- dsOMOPClient:::.v2_plan_schema()
  expect_true(dsOMOPClient:::.jsonSchemaValidate(schema, n)$valid)
})
