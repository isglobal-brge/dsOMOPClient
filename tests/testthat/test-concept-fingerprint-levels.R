# dsOMOP 2.10.8: long public lists travel as fingerprints. Each server sends
# its list's digest; the client reads the list once, and servers holding the
# same list rebuild the shared levels from it. Results equal the full path.
# Fixture: helper-concept-levels.R with a flat 1,000-concept vocabulary.

.fp_vocabulary <- 921000000L + seq_len(1000L)

.fp_setup <- function(observed, env = parent.frame(), rename = NULL) {
  ctx <- .cvl_setup(env = env, deaths = observed)
  for (i in seq_along(ctx$paths)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ctx$paths[[i]])
    names <- paste("Cause", .fp_vocabulary)
    if (!is.null(rename) && i %in% rename) names[[1L]] <- "Renamed cause"
    DBI::dbWriteTable(con, "concept", data.frame(concept_id = .fp_vocabulary, concept_name = names,
      domain_id = "Condition", vocabulary_id = "Flat", concept_class_id = "Cause", standard_concept = "S",
      concept_code = as.character(.fp_vocabulary), valid_start_date = "2000-01-01", valid_end_date = "2099-12-31",
      invalid_reason = NA_character_), append = TRUE)
    DBI::dbDisconnect(con)
  }
  ctx
}

# The shared levels and every site's row values (staged rows read back).
.fp_levels <- function(ctx, target, mode, translate, fingerprint) {
  withr::local_options(list(dsomop.fingerprint_levels = fingerprint))
  recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = translate)),
    out = c(study = target), symbol = "cvl", conns = ctx$conns, output_mode = mode)
  list(levels = .cvl_shared(ctx, target, "cause_of_death_first_value"),
       values = lapply(.cvl_frames(ctx, target), function(frame) as.character(frame$cause_of_death_first_value)))
}

test_that("identical lists rebuild from fingerprints and equal the full path", {
  sixty <- function(offset) rep(.fp_vocabulary[offset + 1:60], each = 2L)
  ctx <- .fp_setup(list(sixty(0L), sixty(100L), sixty(200L)))
  compacted <- 0L
  real <- dsOMOPClient:::.conceptCompactShared
  testthat::local_mocked_bindings(.conceptCompactShared = function(full, listed) {
    compacted <<- compacted + 1L
    real(full, listed)
  }, .package = "dsOMOPClient")
  for (run in list(list("memory", FALSE), list("staged", FALSE), list("memory", TRUE), list("staged", TRUE))) {
    mode <- run[[1L]]; translate <- run[[2L]]
    before <- compacted
    fast <- .fp_levels(ctx, paste0("Fast_", mode, translate), mode, translate, TRUE)
    expect_identical(compacted, before + 1L)
    full <- .fp_levels(ctx, paste0("Full_", mode, translate), mode, translate, FALSE)
    expect_identical(compacted, before + 1L)
    expect_length(fast$levels, 1000L)
    # Same levels and the same value on every row (every person has a cause).
    expect_identical(fast, full)
    expect_false(anyNA(unlist(fast$values)), info = paste(mode, translate))
  }
})

test_that("a site without the list takes the full broadcast", {
  sixty <- function(offset) rep(.fp_vocabulary[offset + 1:60], each = 2L)
  # site2 observes eight causes: within the rule, it declares no list.
  ctx <- .fp_setup(list(sixty(0L), rep(.fp_vocabulary[1:8], each = 5L), sixty(200L)))
  expect_identical(.fp_levels(ctx, "Mixed", "memory", TRUE, TRUE), .fp_levels(ctx, "MixedFull", "memory", TRUE, FALSE))
})

test_that("sites with different lists are read one by one", {
  sixty <- function(offset) rep(.fp_vocabulary[offset + 1:60], each = 2L)
  # site3 names one concept differently: its fingerprint differs.
  ctx2 <- .fp_setup(list(sixty(0L), sixty(100L), sixty(200L)), rename = 3L)
  fast <- .fp_levels(ctx2, "Versions", "memory", TRUE, TRUE)
  expect_identical(fast, .fp_levels(ctx2, "VersionsFull", "memory", TRUE, FALSE))
  expect_length(fast$levels, 1000L)
  expect_false(anyNA(unlist(fast$values)))
})
