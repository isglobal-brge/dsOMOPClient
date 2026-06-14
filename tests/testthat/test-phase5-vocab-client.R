# ==============================================================================
# Tests: Phase 5 client vocabulary / CDM-metadata wrappers (R/vocabulary.R)
#
# These run with no live DataSHIELD backend: a fake session (with a known
# res_symbol) is registered and DSI::datashield.aggregate is mocked so the
# omop*DS() aggregate expression each wrapper builds can be captured and
# asserted. We verify each wrapper (a) targets the right server method with the
# right args and (b) pools correctly (union for catalogs, summed total_count for
# the pager, per-server for cdm.source / cdm.version). Vocabulary / CDM metadata
# is reference data, so these wrappers are intentionally UNGATED (no person
# filter, no banding).
# ==============================================================================

# Decode a B64: transport token exactly as the server's .ds_arg does.
.ds_arg_decode <- function(x) {
  if (!is.character(x) || length(x) != 1L || !startsWith(x, "B64:")) return(x)
  b64 <- substring(x, 5)
  b64 <- gsub("-", "+", b64)
  b64 <- gsub("_", "/", b64)
  pad <- (4 - nchar(b64) %% 4) %% 4
  if (pad > 0) b64 <- paste0(b64, strrep("=", pad))
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)), simplifyVector = TRUE)
}

# Register a fake two-server session with a known res_symbol so .get_session()
# resolves, conns is non-NULL, and the assign symbol in each call is predictable.
.with_fake_vocab_session <- function(symbol = "omop") {
  assign(symbol,
         list(conns = list(s1 = "FAKE1", s2 = "FAKE2"),
              res_symbol = "dsO.test01"),
         envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env)) {
      rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
    }
  )
}

# Run `expr_fn()` with DSI::datashield.aggregate mocked. The mock captures the
# expression and returns `responses[[srv]]` for each (single-server) call, so we
# exercise the real .ds_safe_aggregate per-server loop and the pooling code.
.capture_aggregate <- function(expr_fn, responses = NULL) {
  captured <- NULL
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      captured <<- expr
      srv <- names(conns)[1]
      val <- if (!is.null(responses) && srv %in% names(responses))
        responses[[srv]] else NULL
      stats::setNames(list(val), srv)
    },
    .package = "DSI"
  )
  ret <- expr_fn()
  list(expr = captured, ret = ret)
}

# ------------------------------------------------------------------------------
# Signatures + execute = FALSE dry runs
# ------------------------------------------------------------------------------

test_that("new wrappers expose symbol / conns / execute with expected defaults", {
  for (fn in list(ds.omop.concept.ancestors, ds.omop.concept.synonyms,
                  ds.omop.concept.relationships, ds.omop.concept.list,
                  ds.omop.vocab.vocabularies, ds.omop.vocab.domains,
                  ds.omop.vocab.classes, ds.omop.cdm.source,
                  ds.omop.cdm.version)) {
    args <- formals(fn)
    expect_true("symbol" %in% names(args))
    expect_true("conns" %in% names(args))
    expect_true("execute" %in% names(args))
    expect_equal(args$symbol, "omop")
    expect_null(args$conns)
    expect_true(args$execute)
  }
})

test_that("ds.omop.concept.list has pagination signature", {
  args <- formals(ds.omop.concept.list)
  for (nm in c("domain", "vocabulary", "concept_class", "standard", "valid",
               "offset", "limit", "order")) {
    expect_true(nm %in% names(args))
  }
  expect_null(args$domain)
  expect_null(args$order)
  expect_equal(args$offset, 0)
  expect_equal(args$limit, 100)
})

test_that("execute = FALSE returns a dsomop_result carrying call_code only", {
  r1 <- ds.omop.concept.ancestors(201826, execute = FALSE)
  expect_s3_class(r1, "dsomop_result")
  expect_length(r1$per_site, 0)
  expect_null(r1$pooled)
  expect_true(grepl("ds.omop.concept.ancestors", r1$meta$call_code))

  r2 <- ds.omop.concept.list(domain = "Condition", offset = 100,
                             execute = FALSE)
  expect_s3_class(r2, "dsomop_result")
  expect_true(grepl("ds.omop.concept.list", r2$meta$call_code))
  expect_true(grepl("Condition", r2$meta$call_code))
  expect_true(grepl("offset = 100", r2$meta$call_code))

  r3 <- ds.omop.cdm.version(execute = FALSE)
  expect_true(grepl("ds.omop.cdm.version", r3$meta$call_code))
})

# ------------------------------------------------------------------------------
# Concept hierarchy / synonyms / relationships -> correct DSI call + union pool
# ------------------------------------------------------------------------------

test_that("ds.omop.concept.ancestors builds omopConceptAncestorsDS and unions", {
  .with_fake_vocab_session()
  resp <- list(
    s1 = data.frame(direction = c("ancestor", "descendant"),
                    concept_id = c(441840L, 4034964L),
                    levels_of_separation = c(1L, 1L),
                    stringsAsFactors = FALSE),
    # s2 repeats the ancestor (dedupe) and adds one new descendant
    s2 = data.frame(direction = c("ancestor", "descendant"),
                    concept_id = c(441840L, 9999L),
                    levels_of_separation = c(1L, 2L),
                    stringsAsFactors = FALSE)
  )
  out <- .capture_aggregate(
    function() ds.omop.concept.ancestors(c(201826, 4329847)), resp)

  expect_equal(as.character(out$expr[[1]]), "omopConceptAncestorsDS")
  expect_equal(out$expr[[2]], "dsO.test01")
  expect_equal(sort(as.integer(.ds_arg_decode(out$expr[[3]]))),
               c(201826L, 4329847L))

  # Pooled = de-duplicated union; the shared ancestor row collapses to one.
  pooled <- out$ret$pooled
  expect_s3_class(pooled, "data.frame")
  expect_true("levels_of_separation" %in% names(pooled))
  expect_equal(nrow(pooled), 3)
  expect_setequal(pooled$concept_id, c(441840L, 4034964L, 9999L))
})

test_that("ds.omop.concept.synonyms builds omopConceptSynonymsDS", {
  .with_fake_vocab_session()
  out <- .capture_aggregate(function() ds.omop.concept.synonyms(201826))
  expect_equal(as.character(out$expr[[1]]), "omopConceptSynonymsDS")
  expect_equal(out$expr[[2]], "dsO.test01")
  # Single id -> .ds_encode leaves it bare (no B64 transport).
  expect_equal(as.integer(out$expr[[3]]), 201826L)
})

test_that("ds.omop.concept.relationships forwards an optional relationship_id", {
  .with_fake_vocab_session()
  # Default: relationship_id NULL.
  out <- .capture_aggregate(function() ds.omop.concept.relationships(201826))
  expect_equal(as.character(out$expr[[1]]), "omopConceptRelationshipsDS")
  expect_null(out$expr$relationship_id)

  # With a single-relationship filter (string passes through bare).
  out2 <- .capture_aggregate(
    function() ds.omop.concept.relationships(201826,
                                             relationship_id = "Maps to"))
  expect_equal(out2$expr$relationship_id, "Maps to")
})

# ------------------------------------------------------------------------------
# Paged concept catalog -> total_count surfaced + summed, rows paginated/unioned
# ------------------------------------------------------------------------------

test_that("ds.omop.concept.list passes OFFSET/LIMIT + filters and sums totals", {
  .with_fake_vocab_session()
  resp <- list(
    s1 = list(rows = data.frame(concept_id = c(1L, 2L),
                                concept_name = c("a", "b"),
                                stringsAsFactors = FALSE),
              total_count = 500, offset = 100L, limit = 2L),
    s2 = list(rows = data.frame(concept_id = c(2L, 3L),
                                concept_name = c("b", "c"),
                                stringsAsFactors = FALSE),
              total_count = 300, offset = 100L, limit = 2L)
  )
  out <- .capture_aggregate(
    function() ds.omop.concept.list(domain = "Condition",
                                    vocabulary = "SNOMED",
                                    offset = 100, limit = 2), resp)

  # Correct server method + paginated, filtered call.
  expect_equal(as.character(out$expr[[1]]), "omopListConceptsDS")
  expect_equal(out$expr[[2]], "dsO.test01")
  expect_equal(out$expr$domain, "Condition")
  expect_equal(out$expr$vocabulary, "SNOMED")
  expect_equal(out$expr$offset, 100L)
  expect_equal(out$expr$limit, 2L)

  # per_site keeps each site's total_count; pooled sums them and unions rows.
  expect_equal(out$ret$per_site$s1$total_count, 500)
  expect_equal(out$ret$pooled$total_count, 800)
  expect_equal(out$ret$pooled$offset, 100L)
  expect_equal(out$ret$pooled$limit, 2L)
  # Concept_id 2 is shared across both sites -> de-duplicated in the union.
  expect_setequal(out$ret$pooled$rows$concept_id, c(1L, 2L, 3L))
})

# ------------------------------------------------------------------------------
# Vocabulary catalogs (vocabularies / domains / classes) -> union pool
# ------------------------------------------------------------------------------

test_that("vocab catalog listers build the right method and union per site", {
  cases <- list(
    list(fn = ds.omop.vocab.vocabularies, method = "omopVocabulariesDS",
         col = "vocabulary_id"),
    list(fn = ds.omop.vocab.domains, method = "omopDomainsDS",
         col = "domain_id"),
    list(fn = ds.omop.vocab.classes, method = "omopConceptClassesDS",
         col = "concept_class_id")
  )
  for (case in cases) {
    .with_fake_vocab_session()
    df1 <- stats::setNames(
      data.frame(x = c("SNOMED", "RxNorm"), stringsAsFactors = FALSE),
      case$col)
    df2 <- stats::setNames(
      data.frame(x = c("RxNorm", "LOINC"), stringsAsFactors = FALSE),
      case$col)
    out <- .capture_aggregate(function() case$fn(),
                              list(s1 = df1, s2 = df2))
    expect_equal(as.character(out$expr[[1]]), case$method)
    expect_equal(out$expr[[2]], "dsO.test01")
    # Union of {SNOMED,RxNorm} and {RxNorm,LOINC} -> 3 distinct rows.
    expect_equal(nrow(out$ret$pooled), 3)
    expect_setequal(out$ret$pooled[[case$col]],
                    c("SNOMED", "RxNorm", "LOINC"))
  }
})

# ------------------------------------------------------------------------------
# concept.search extension: concept_id + standard + valid filters
# ------------------------------------------------------------------------------

test_that("ds.omop.concept.search accepts concept_id / standard / valid", {
  args <- formals(ds.omop.concept.search)
  # pattern is now optional (NULL default) for backward-compatible widening.
  expect_null(args$pattern)
  for (nm in c("concept_id", "standard", "valid")) {
    expect_true(nm %in% names(args))
    expect_null(args[[nm]])
  }

  .with_fake_vocab_session()
  out <- .capture_aggregate(
    function() ds.omop.concept.search(NULL, concept_id = c(201826, 4329847),
                                      standard = "S", valid = TRUE))
  expect_equal(as.character(out$expr[[1]]), "omopSearchConceptsDS")
  # Extended named filters forwarded to the extended server method.
  expect_equal(sort(as.integer(.ds_arg_decode(out$expr$concept_id))),
               c(201826L, 4329847L))
  expect_equal(out$expr$standard, "S")
  expect_true(out$expr$valid)
})

test_that("ds.omop.concept.search stays backward-compatible (pattern only)", {
  .with_fake_vocab_session()
  out <- .capture_aggregate(
    function() ds.omop.concept.search("diabetes", domain = "Condition"))
  expect_equal(as.character(out$expr[[1]]), "omopSearchConceptsDS")
  # Positional: res_symbol, pattern, domain, vocabulary, standard_only, limit.
  expect_equal(out$expr[[2]], "dsO.test01")
  expect_equal(out$expr[[3]], "diabetes")
  expect_equal(out$expr[[4]], "Condition")
  # No id filter -> concept_id arg omitted (NULL).
  expect_null(out$expr$concept_id)
})

# ------------------------------------------------------------------------------
# CDM source / version -> per-server (never silently merged)
# ------------------------------------------------------------------------------

test_that("ds.omop.cdm.source returns per-server, no pooled merge", {
  .with_fake_vocab_session()
  resp <- list(
    s1 = data.frame(cdm_source_name = "Site 1", cdm_version = "5.3",
                    stringsAsFactors = FALSE),
    s2 = data.frame(cdm_source_name = "Site 2", cdm_version = "5.4",
                    stringsAsFactors = FALSE)
  )
  out <- .capture_aggregate(function() ds.omop.cdm.source(), resp)
  expect_equal(as.character(out$expr[[1]]), "omopCdmSourceDS")
  expect_equal(out$expr[[2]], "dsO.test01")
  # Per-server: pooled is NULL, both sites preserved distinctly.
  expect_null(out$ret$pooled)
  expect_equal(out$ret$meta$scope, "per_site")
  expect_equal(out$ret$per_site$s1$cdm_version, "5.3")
  expect_equal(out$ret$per_site$s2$cdm_version, "5.4")
})

test_that("ds.omop.cdm.version returns divergent versions per server", {
  .with_fake_vocab_session()
  resp <- list(
    s1 = list(cdm_version = "5.3", source = "cdm_source",
              vocabulary_version = "v5.0 31-AUG-22"),
    s2 = list(cdm_version = "5.4", source = "structure",
              vocabulary_version = NA_character_)
  )
  out <- .capture_aggregate(function() ds.omop.cdm.version(), resp)
  expect_equal(as.character(out$expr[[1]]), "omopCdmVersionDS")
  expect_null(out$ret$pooled)
  # Divergent versions are surfaced per server, not collapsed to one.
  expect_equal(out$ret$per_site$s1$cdm_version, "5.3")
  expect_equal(out$ret$per_site$s2$cdm_version, "5.4")
})
