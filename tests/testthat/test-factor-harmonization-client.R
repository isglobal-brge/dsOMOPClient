# Client side of the concept-factor coordination layer: the factor_concepts
# plan option and the pure level-union reduction (.unionConceptLevels) that
# turns each server's safe-level report into one shared, ordered spec.

# --- factor_concepts plan option ---------------------------------------------

test_that("factor_concepts defaults to TRUE on a fresh plan", {
  plan <- ds.omop.plan()
  expect_true(plan$options$factor_concepts)
})

test_that("plan.options can disable factor_concepts", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.options(plan, factor_concepts = FALSE)
  expect_false(plan$options$factor_concepts)
})

test_that("plan.options leaves factor_concepts untouched when omitted", {
  plan <- ds.omop.plan()
  plan <- ds.omop.plan.options(plan, translate_concepts = TRUE)
  expect_true(plan$options$factor_concepts)
})

test_that("print.omop_plan surfaces factor_concepts", {
  plan <- ds.omop.plan()
  output <- capture.output(print(plan))
  expect_true(any(grepl("factor_concepts=", output)))
})

# --- .ds_encode round-trip of a harmonization spec ---------------------------

test_that(".ds_encode preserves a spec's column names and level order", {
  ds_arg_decode <- function(x) {
    b64 <- substring(x, 5)
    b64 <- gsub("-", "+", b64)
    b64 <- gsub("_", "/", b64)
    pad <- (4 - nchar(b64) %% 4) %% 4
    if (pad > 0) b64 <- paste0(b64, strrep("=", pad))
    jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)),
                       simplifyVector = FALSE)
  }
  spec <- list(
    gender_concept_id = c("8507", "8532", "9999"),
    race_concept_id = c("8516", "8527")
  )
  decoded <- ds_arg_decode(dsOMOPClient:::.ds_encode(spec))
  expect_equal(sort(names(decoded)),
               c("gender_concept_id", "race_concept_id"))
  expect_equal(unlist(decoded$gender_concept_id), c("8507", "8532", "9999"))
  expect_equal(unlist(decoded$race_concept_id), c("8516", "8527"))
})

# --- .unionConceptLevels: the cross-server reduction -------------------------

test_that("unionConceptLevels unions numeric ids in numeric order", {
  per_server <- list(
    a = list(levels = list(gender_concept_id = c("8532", "8507")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = list(levels = list(gender_concept_id = c("8507", "9999")),
             unsafe = character(0), nfilter_levels_max = 40)
  )
  spec <- dsOMOPClient:::.unionConceptLevels(per_server)
  expect_equal(spec$gender_concept_id, c("8507", "8532", "9999"))
})

test_that("unionConceptLevels sorts non-numeric labels by radix", {
  per_server <- list(
    a = list(levels = list(x_concept_id = c("male", "female")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = list(levels = list(x_concept_id = c("female", "other")),
             unsafe = character(0), nfilter_levels_max = 40)
  )
  spec <- dsOMOPClient:::.unionConceptLevels(per_server)
  expect_equal(spec$x_concept_id, c("female", "male", "other"))
})

test_that("unionConceptLevels drops a column unsafe on any one server", {
  per_server <- list(
    a = list(levels = list(c_concept_id = c("1", "2", "3")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = list(levels = list(),
             unsafe = "c_concept_id", nfilter_levels_max = 40)
  )
  spec <- dsOMOPClient:::.unionConceptLevels(per_server)
  expect_false("c_concept_id" %in% names(spec))
  expect_equal(length(spec), 0L)
})

test_that("unionConceptLevels honors the smallest server cap", {
  per_server <- list(
    a = list(levels = list(x_concept_id = c("1", "2", "3", "4")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = list(levels = list(x_concept_id = c("1", "2")),
             unsafe = character(0), nfilter_levels_max = 3)
  )
  # Union has 4 levels but the tightest server cap is 3 => drop the column.
  spec <- dsOMOPClient:::.unionConceptLevels(per_server)
  expect_false("x_concept_id" %in% names(spec))
})

test_that("unionConceptLevels harmonizes a column present on only one server", {
  per_server <- list(
    a = list(levels = list(gender_concept_id = c("8532", "8507")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = list(levels = list(),
             unsafe = character(0), nfilter_levels_max = 40)
  )
  spec <- dsOMOPClient:::.unionConceptLevels(per_server)
  expect_equal(spec$gender_concept_id, c("8507", "8532"))
})

test_that("unionConceptLevels ignores NULL server entries", {
  per_server <- list(
    a = list(levels = list(g_concept_id = c("2", "1")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = NULL
  )
  spec <- dsOMOPClient:::.unionConceptLevels(per_server)
  expect_equal(spec$g_concept_id, c("1", "2"))
})

test_that("unionConceptLevels returns an empty list when nothing is harmonizable", {
  expect_equal(dsOMOPClient:::.unionConceptLevels(list()), list())
  expect_equal(
    dsOMOPClient:::.unionConceptLevels(list(
      a = list(levels = list(), unsafe = character(0),
               nfilter_levels_max = 40)
    )),
    list()
  )
})

test_that("unionConceptLevels falls back to cap 40 when a server omits it", {
  per_server <- list(
    a = list(levels = list(x_concept_id = as.character(1:30)),
             unsafe = character(0), nfilter_levels_max = NULL)
  )
  spec <- dsOMOPClient:::.unionConceptLevels(per_server)
  # 30 <= fallback cap 40, so it survives.
  expect_equal(length(spec$x_concept_id), 30L)
})
