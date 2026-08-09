test_that("local QueryLibrary sticky catalog is complete and fail-closed", {
  executable <- omop_querylibrary_sticky_catalog()
  all <- omop_querylibrary_sticky_catalog(include_unavailable = TRUE)

  expect_equal(nrow(executable), 129L)
  expect_equal(nrow(all), 201L)
  expect_equal(sum(all$status == "metadata_reference"), 54L)
  expect_equal(sum(all$status == "held_back"), 0L)
  expect_equal(sum(all$status == "blocked"), 18L)
  expect_identical(anyDuplicated(all$upstream_id), 0L)
  expect_setequal(
    unique(executable$statistic),
    c("count", "bounded_record_count", "categorical_histogram",
      "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate")
  )
  expect_true(all(executable$source_commit ==
                    "df8a21074b08519e581ca1afb7510468538117a4"))
  expect_true(all(!all$literal_upstream_sql_authorized))
  expect_setequal(
    all$upstream_id,
    c(executable$upstream_id,
      names(.querylibrary_sticky_unavailable()$metadata_reference),
      names(.querylibrary_sticky_unavailable()$blocked))
  )
  cs01 <- all[all$upstream_id == "CS01", , drop = FALSE]
  expect_identical(cs01$status, "metadata_reference")
  expect_match(cs01$reason, "institutional care_site aggregate")
})

test_that("each mapped primitive builds a canonical privacy specification", {
  count <- omop_querylibrary_sticky("CO11")
  categorical <- omop_querylibrary_sticky(
    "PE03", variable = "gender", levels = c("F", "M", "Unknown")
  )
  numeric <- omop_querylibrary_sticky(
    "PE12", variable = "birth_year", breaks = seq(1900, 2030, 10)
  )
  first <- omop_querylibrary_sticky(
    "CO02", variable = "onset_date",
    breaks = c("2010-01-01", "2020-01-01", "2030-01-01")
  )
  records <- omop_querylibrary_sticky(
    "CO19", variable = "month", breaks = seq(0.5, 12.5),
    max_contributions = 12L, order_by = "event_date"
  )
  record_count <- omop_querylibrary_sticky(
    "CO10", max_contributions = 4L
  )
  categorical_records <- omop_querylibrary_sticky(
    "CE09", variable = "category", levels = c("a", "b"),
    max_contributions = 4L, order_by = "event_date"
  )
  distinct <- omop_querylibrary_sticky(
    "DEX06", variable = "concept_id", levels = c(10L, 20L, 30L),
    max_contributions = 2L
  )
  mean <- omop_querylibrary_sticky(
    "OP05", variable = "months", lower = 0, upper = 240
  )
  rate <- omop_querylibrary_sticky(
    "DEX14", variable = "overlap", positive = "yes"
  )

  values <- list(
    count, categorical, numeric, first, records, record_count,
    categorical_records, distinct, mean, rate
  )
  expect_true(all(vapply(values, inherits, logical(1L),
                         "omop_querylibrary_sticky")))
  expect_true(all(vapply(values, function(x) {
    inherits(x$privacy, "omop_privacy") &&
      identical(x$mapping$literal_upstream_sql_authorized, FALSE)
  }, logical(1L))))
  expect_identical(
    names(formals(ds.omop.querylibrary.sticky.release)),
    c("x", "redesign", "datasources", "pool", "format")
  )
  expect_identical(count$privacy$statistic, "count")
  expect_identical(categorical$privacy$reducer, "presence")
  expect_identical(numeric$privacy$reducer, "mean")
  expect_identical(first$privacy$reducer, "first")
  expect_identical(first$privacy$order_by, "onset_date")
  expect_identical(records$privacy$reducer, "records")
  expect_identical(records$privacy$max_contributions, 12L)
  expect_identical(record_count$privacy$statistic, "bounded_record_count")
  expect_identical(record_count$privacy$max_contributions, 4L)
  expect_identical(categorical_records$privacy$reducer, "records")
  expect_identical(distinct$privacy$statistic, "bounded_distinct")
  expect_identical(distinct$privacy$levels, c("10", "20", "30"))
  expect_identical(mean$privacy$statistic, "bounded_mean")
  expect_identical(rate$privacy$denominator, "all_persons")
})

test_that("all 129 catalog mappings compile through bounded primitives", {
  catalog <- omop_querylibrary_sticky_catalog()
  for (i in seq_len(nrow(catalog))) {
    entry <- catalog[i, , drop = FALSE]
    args <- switch(
      entry$statistic,
      count = list(),
      bounded_record_count = list(max_contributions = 2L),
      categorical_histogram = list(
        variable = "category", levels = c("a", "b"),
        max_contributions = 1L,
        order_by = if (entry$order_by_required) "event_date" else NULL
      ),
      numeric_histogram = list(
        variable = "value", breaks = c(0, 1, 2),
        max_contributions = if (entry$record_cap_required) 2L else NULL,
        order_by = if (entry$order_by_required) "event_date" else NULL
      ),
      bounded_distinct = list(
        variable = "concept_id", levels = c(10L, 20L),
        max_contributions = 2L
      ),
      bounded_mean = list(variable = "value", lower = 0, upper = 1),
      binary_rate = list(variable = "outcome", positive = "yes")
    )
    value <- do.call(
      omop_querylibrary_sticky,
      c(list(upstream_id = entry$upstream_id), args)
    )
    expect_identical(value$mapping$statistic, entry$statistic,
                     info = entry$upstream_id)
    expect_identical(value$mapping$reducer, entry$reducer,
                     info = entry$upstream_id)
  }
})

test_that("bounded mappings require explicit caps and blocked shapes fail", {
  expect_error(
    omop_querylibrary_sticky(
      "DEX06", variable = "concept_id", levels = c(10L, 20L)
    ),
    "max_contributions is required"
  )
  expect_error(
    omop_querylibrary_sticky(
      "CE09", variable = "category", levels = c("a", "b")
    ),
    "order_by is required"
  )
  expect_error(
    omop_querylibrary_sticky("CO10"),
    "max_contributions is required"
  )
  expect_error(omop_querylibrary_sticky("PE08"), "blocked")
  expect_error(omop_querylibrary_sticky("CO20"), "blocked")
  expect_error(omop_querylibrary_sticky("DER02"), "blocked")
  expect_error(omop_querylibrary_sticky("C01"), "metadata/reference")
  expect_error(omop_querylibrary_sticky("CS01"), "metadata/reference")
  expect_error(
    omop_querylibrary_sticky(
      "COC01", variable = "therapy", levels = c("drug", "surgery")
    ),
    "order_by is required"
  )

  expect_error(
    omop_querylibrary_sticky(
      "CO19", variable = "month", breaks = seq(0.5, 12.5),
      order_by = "event_date"
    ),
    "max_contributions is required"
  )
  expect_error(
    omop_querylibrary_sticky(
      "CO19", variable = "month", breaks = seq(0.5, 12.5),
      max_contributions = 12L
    ),
    "order_by is required"
  )
})

test_that("optional Recipe preparation must have exactly one output", {
  one <- omop_recipe(
    variables = omop_variable_age(),
    outputs = omop_output(name = "persons", type = "wide")
  )
  value <- omop_querylibrary_sticky("CO11", preparation = one)
  expect_identical(value$preparation, one)

  two <- omop_recipe(
    variables = omop_variable_age(),
    outputs = list(
      omop_output(name = "one", type = "wide"),
      omop_output(name = "two", type = "wide")
    )
  )
  expect_error(
    omop_querylibrary_sticky("CO11", preparation = two),
    "exactly one protected output"
  )
  expect_error(
    omop_querylibrary_sticky("CO11", preparation = list()),
    "omop_recipe or omop_plan"
  )
})

test_that("release verifies the pinned mapping on every server", {
  redesign <- omop_querylibrary_sticky("CO11")
  server_catalog <- omop_querylibrary_sticky_catalog()
  server_catalog$literal_sql_authorized <- FALSE
  released <- NULL

  testthat::local_mocked_bindings(
    .dp_datasources = function(datasources) datasources,
    .dp_complete_aggregate = function(datasources, expr, operation) {
      stats::setNames(lapply(datasources, function(x) server_catalog),
                      names(datasources))
    },
    ds.omop.dp.release = function(x, privacy, datasources, pool, format) {
      released <<- list(x = x, privacy = privacy, datasources = datasources,
                        pool = pool, format = format)
      structure(list(ok = TRUE), class = "dsomop_result")
    },
    .package = "dsOMOPClient"
  )

  result <- ds.omop.querylibrary.sticky.release(
    "prepared", redesign, datasources = list(a = "A", b = "B"),
    pool = FALSE, format = "raw"
  )
  expect_s3_class(result, "dsomop_result")
  expect_identical(released$x, "prepared")
  expect_identical(released$privacy, redesign$privacy)
  expect_identical(names(released$datasources), c("a", "b"))
})

test_that("release rejects a locally mutated redesign before server access", {
  redesign <- omop_querylibrary_sticky("CO11")
  redesign$privacy <- omop_privacy(
    "bounded_record_count", max_contributions = 2L
  )

  expect_error(
    ds.omop.querylibrary.sticky.release(
      "prepared", redesign, datasources = list(a = "A")
    ),
    "does not match its pinned QueryLibrary mapping"
  )
})

test_that("release rejects a stale or different server mapping", {
  redesign <- omop_querylibrary_sticky("CO11")
  server_catalog <- omop_querylibrary_sticky_catalog()
  server_catalog$literal_sql_authorized <- FALSE
  server_catalog$source_commit <- "stale"

  testthat::local_mocked_bindings(
    .dp_datasources = function(datasources) datasources,
    .dp_complete_aggregate = function(datasources, expr, operation) {
      list(a = server_catalog)
    },
    .package = "dsOMOPClient"
  )
  expect_error(
    ds.omop.querylibrary.sticky.release(
      "prepared", redesign, datasources = list(a = "A")
    ),
    "does not advertise"
  )
})
