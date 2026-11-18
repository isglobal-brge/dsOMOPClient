# Client side of the concept-factor coordination layer: the factor_concepts
# plan option and the pure level-union reduction (.unionConceptLevels) that
# turns each server's safe-level report into one shared, ordered spec.

# --- factor_concepts plan option ---------------------------------------------

test_that("factor_concepts defaults to TRUE on a fresh plan", {
  plan <- ds.omop.plan()
  expect_true(plan$options$factor_concepts)
})

test_that("factor_concepts FALSE is rejected in both translation modes", {
  for (translate in c(TRUE, FALSE)) {
    expect_error(ds.omop.plan.options(ds.omop.plan(),
      translate_concepts = translate, factor_concepts = FALSE),
      "concept IDs are always categories since 2.10.6", fixed = TRUE)
    expect_error(recipe_set_options(omop_recipe(),
      translate_concepts = translate, factor_concepts = FALSE),
      "concept IDs are always categories since 2.10.6", fixed = TRUE)
  }
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

test_that("unionConceptLevels refuses a column unsafe on any one server", {
  per_server <- list(
    a = list(levels = list(c_concept_id = c("1", "2", "3")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = list(levels = list(),
             unsafe = "c_concept_id", nfilter_levels_max = 40)
  )
  per_server$b$unsafe_rules <- list(c_concept_id = "nfilter.levels.density")
  expect_error(dsOMOPClient:::.unionConceptLevels(per_server),
    "column 'c_concept_id' on site 'b'.*nfilter.levels.density.*Declare.*omop_category_recode.*concepts =")
})

test_that("unionConceptLevels applies no shared union cap", {
  per_server <- list(
    a = list(levels = list(x_concept_id = c("1", "2", "3", "4")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = list(levels = list(x_concept_id = c("1", "2")),
             unsafe = character(0), nfilter_levels_max = 3)
  )
  spec <- dsOMOPClient:::.unionConceptLevels(per_server)
  expect_identical(spec$x_concept_id, as.character(1:4))
})

test_that("unionConceptLevels refuses incompatible column schemas", {
  per_server <- list(
    a = list(levels = list(gender_concept_id = c("8532", "8507")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = list(levels = list(),
             unsafe = character(0), nfilter_levels_max = 40)
  )
  expect_error(dsOMOPClient:::.unionConceptLevels(per_server), "Incompatible concept-column schemas")
})

test_that("unionConceptLevels rejects NULL server entries", {
  per_server <- list(
    a = list(levels = list(g_concept_id = c("2", "1")),
             unsafe = character(0), nfilter_levels_max = 40),
    b = NULL
  )
  expect_error(dsOMOPClient:::.unionConceptLevels(per_server), "incomplete")
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

test_that("unionConceptLevels does not require an assignment cap", {
  per_server <- list(
    a = list(levels = list(x_concept_id = as.character(1:30)),
             unsafe = character(0), nfilter_levels_max = NULL)
  )
  spec <- dsOMOPClient:::.unionConceptLevels(per_server)
  # Only the per-site observed-level report has a level-count rule.
  expect_equal(length(spec$x_concept_id), 30L)
})

test_that("factor harmonization fails closed on unequal component sets", {
  removed <- character(0)
  conns <- list(a = "A", b = "B")
  state <- list(a = c("D", "D.covariateRef"), b = "D")
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], symbol)
      removed <<- c(removed, symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )
  expect_error(
    dsOMOPClient:::.harmonizeConceptFactors(
      list(result = c("D", "D.covariateRef")), conns
    ),
    "exact component set"
  )
  expect_true(all(c("D", "D.covariateRef") %in% removed))
})

test_that("factor harmonization removes outputs when level collection fails", {
  removed <- character(0)
  conns <- list(a = "A", b = "B")
  state <- list(a = "D", b = "D")
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.aggregate = function(conns, expr, ...) {
      stop("aggregate unavailable")
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], symbol)
      removed <<- c(removed, symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )
  expect_error(
    dsOMOPClient:::.harmonizeConceptFactors(c(result = "D"), conns),
    "freshly assigned outputs were removed"
  )
  expect_true("D" %in% removed)
})

test_that("factor harmonization rejects a NULL result from one server", {
  removed <- character(0)
  conns <- list(a = "A", b = "B")
  state <- list(a = "D", b = "D")
  report <- list(levels = list(gender_concept_id = c("8507", "8532")),
                 unsafe = character(0), nfilter_levels_max = 40)
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.aggregate = function(conns, expr, success = NULL,
                                    error = NULL, ...) {
      success("a", report)
      list(a = report, b = NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], symbol)
      removed <<- c(removed, symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )
  expect_error(
    dsOMOPClient:::.harmonizeConceptFactors(c(result = "D"), conns),
    "freshly assigned outputs were removed"
  )
  expect_true("D" %in% removed)
})

test_that("factor harmonization verifies assign success on every server", {
  removed <- character(0)
  conns <- list(a = "A", b = "B")
  state <- list(a = "D", b = "D")
  report <- list(levels = list(gender_concept_id = c("8507", "8532")),
                 unsafe = character(0), nfilter_levels_max = 40)
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) state[names(conns)],
    datashield.aggregate = function(conns, expr, success = NULL,
                                    error = NULL, ...) {
      success("a", report)
      success("b", report)
      list(a = report, b = report)
    },
    datashield.assign.expr = function(conns, symbol, expr, success = NULL,
                                      error = NULL, ...) {
      success("a")
      error("b", "assignment failed")
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      server <- names(conns)[[1L]]
      state[[server]] <<- setdiff(state[[server]], symbol)
      removed <<- c(removed, symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )
  expect_error(
    dsOMOPClient:::.harmonizeConceptFactors(c(result = "D"), conns),
    "freshly assigned outputs were removed"
  )
  expect_true("D" %in% removed)
})

test_that("factor harmonization ignores unrelated prefixed user symbols", {
  aggregated <- character(0)
  conns <- list(a = "A", b = "B")
  report <- list(levels = list(), unsafe = character(0),
                 nfilter_levels_max = 40)
  testthat::local_mocked_bindings(
    datashield.symbols = function(conns, ...) list(
      a = c("D", "D.model"), b = c("D", "D.model")
    ),
    datashield.aggregate = function(conns, expr, success = NULL,
                                    error = NULL, ...) {
      aggregated <<- c(aggregated, deparse(expr))
      success("a", report)
      success("b", report)
      list(a = report, b = report)
    },
    .package = "DSI"
  )
  expect_null(dsOMOPClient:::.harmonizeConceptFactors(
    c(result = "D"), conns
  ))
  expect_length(aggregated, 1L)
  expect_match(aggregated, "D")
  expect_false(any(grepl("D.model", aggregated, fixed = TRUE)))
})

test_that("legacy all-missing concept columns retain an empty factor contract", {
  report <- list(levels = list(cause_concept_id = character()), unsafe = character())
  expect_identical(.unionConceptLevels(list(a = report, b = report)),
                   list(cause_concept_id = character()))
})


test_that("factor transport preserves absent numeric metadata fields", {
  expect_null(.categoryMetadataPlain(NULL))
  spec <- list(concept_protocol = 1L, columns = list(),
    numbers = list(days = list(input_type = "integer", source = NULL,
      levels = NULL, pipeline = list(list(kind = "transform", method = "identity")))))
  decoded <- .decode_ds(.ds_encode(spec))
  expect_identical(decoded$numbers, spec$numbers)
})


test_that("publication needs a supported server version and concept identities", {
  session <- list(capabilities = list(site = .caps()), server_versions = c(site = "2.10.6"))
  expect_silent(.conceptAssertSupport(session, "site"))
  session$server_versions <- c(site = "2.11.0")
  expect_silent(.conceptAssertSupport(session, "site"))
  for (version in list(NULL, NA_character_, "unknown", "2.10.5")) {
    session$server_versions <- if (is.null(version)) character() else c(site = version)
    expect_error(.conceptAssertSupport(session, "site"), "2.10.6 or later on: site.*always categories")
  }
  session <- list(capabilities = list(site = .caps(ops = character())),
                  server_versions = c(site = "2.10.6"))
  expect_error(.conceptAssertSupport(session, "site"), "Concept factors require server support.*site")
})
