test_that("vocabulary fallbacks retain metadata released by other servers", {
  raw <- list(
    site_a = data.frame(vocabulary_id = "A"),
    site_b = data.frame(vocabulary_id = "A", vocabulary_name = "Reference A",
                        vocabulary_concept_id = 1L),
    site_c = data.frame(vocabulary_id = "B", vocabulary_name = "Reference B",
                        vocabulary_concept_id = 2L)
  )
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = setNames(as.list(seq_along(raw)), names(raw)),
           res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) raw,
    .package = "dsOMOPClient"
  )
  readers <- list(ds.omop.vocab.vocabularies, ds.omop.vocab.domains,
                  ds.omop.vocab.classes)
  for (reader in readers) {
    result <- reader(type = "both")
    expect_named(result$pooled, names(raw$site_b))
    expect_identical(result$pooled$vocabulary_name,
                     c(NA_character_, "Reference A", "Reference B"))
    expect_identical(result$pooled$vocabulary_concept_id, c(NA_integer_, 1L, 2L))
    expect_equal(nrow(as.data.frame(result)), 3L)
  }
})

test_that("empty metadata frames retain columns without fabricating rows", {
  raw <- list(
    site_a = data.frame(id = integer()),
    site_b = data.frame(id = integer(), label = character(),
                        valid_start_date = as.Date(character()))
  )
  pooled <- .pool_vocab_union(raw)
  expect_identical(nrow(pooled), 0L)
  expect_named(pooled, names(raw$site_b))
  expect_type(pooled$label, "character")
  expect_s3_class(pooled$valid_start_date, "Date")
  expect_null(.pool_vocab_union(c(raw, list(site_c = NULL))))
  attr(raw, "ds_errors") <- list(site_c = "unavailable")
  expect_null(.pool_vocab_union(raw))
})

test_that("common schema listings do not claim first-server agreement", {
  raw <- list(
    site_a = data.frame(column_name = c("x", "only_a"),
                        data_type = c("REAL", "TEXT"),
                        is_nullable = c(TRUE, TRUE)),
    site_b = data.frame(column_name = "x", data_type = "INTEGER",
                        is_nullable = FALSE, ordinal_position = 1L),
    site_c = data.frame(column_name = "x", data_type = "REAL",
                        is_nullable = TRUE, ordinal_position = 1L)
  )
  pooled <- .dictionary_common_rows(raw, "column_name", names(raw))
  expect_identical(pooled$column_name, "x")
  expect_identical(pooled$data_type, NA_character_)
  expect_identical(pooled$is_nullable, NA)
  expect_identical(pooled$ordinal_position, NA_integer_)
  expect_identical(.dictionary_common_rows(raw[1L], "column_name", "site_a"),
                   raw$site_a)
  expect_null(.dictionary_common_rows(raw, "column_name", c(names(raw), "absent")))
})

test_that("concept-page bounds require all requested totals and pages", {
  page <- function(total) list(rows = data.frame(concept_id = 1L),
                               total_count = total, offset = 0L, limit = 100L)
  raw <- list(site_a = page(20L), site_b = page(NA_real_), site_c = page(30L))
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(site_a = 1, site_b = 2, site_c = 3),
           res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) raw,
    .package = "dsOMOPClient"
  )
  expect_null(ds.omop.concept.list(type = "both")$pooled)
  expect_error(ds.omop.concept.list(type = "combine"), "Not every server")
  raw$site_b <- page(10L)
  complete <- ds.omop.concept.list(type = "both")
  expect_identical(complete$pooled$total_count, 60)
  expect_identical(complete$pooled$total_count_max, 30)
  raw$site_b$rows <- NULL
  expect_null(ds.omop.concept.list(type = "both")$pooled)
  raw$site_b <- NULL
  expect_null(ds.omop.concept.list(type = "both")$pooled)
  attr(raw, "ds_errors") <- list(site_b = "unavailable")
  expect_error(ds.omop.concept.list(type = "combine"), "Not every server")
})

test_that("concept summaries convert without recycling nested result rows", {
  counts <- dsomop_result(
    per_site = list(site_a = data.frame(value = c("A", "B"), n = c(10, 20))),
    pooled = data.frame(value = c("A", "B"), n = c(10, 20)))
  testthat::local_mocked_bindings(
    ds.omop.value.counts = function(...) counts,
    .package = "dsOMOPClient"
  )
  for (type in c("split", "both")) {
    result <- ds.omop.concept.summary("observation", 100L,
      column = "value_as_concept_id", type = type)
    frame <- as.data.frame(result)
    expect_equal(nrow(frame), 1L)
    expect_identical(frame$numeric, NA)
    expect_identical(frame$categorical[[1L]]$value_as_concept_id, counts)
    expect_output(print(result), "observation")
  }
  expect_error(ds.omop.concept.summary("observation", 100L,
    column = "value_as_concept_id", type = "combine"), "quantiles")
})

test_that("OHDSI table catalogs retain heterogeneous released fields", {
  raw <- list(
    site_a = data.frame(table_name = "cohort_count", n_rows = 10),
    site_b = data.frame(table_name = "cohort_count", n_rows = 20,
                        tool_name = "Cohort diagnostics"),
    site_c = data.frame(table_name = "cohort_count", n_rows = 30,
                        tool_name = "Cohort diagnostics")
  )
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = setNames(as.list(seq_along(raw)), names(raw)),
           res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) raw,
    .package = "dsOMOPClient"
  )
  for (type in c("split", "combine", "both")) {
    result <- ds.omop.ohdsi.tables(type = type)
    frame <- as.data.frame(result)
    expect_identical(frame$n_rows, c(10, 20, 30))
    expect_identical(frame$tool_name,
                     c(NA_character_, rep("Cohort diagnostics", 2)))
    expect_output(print(result), "cohort_count")
  }
})
