# ==============================================================================
# Tests for client OHDSI Results wrappers
# ==============================================================================

# --- Function signature tests -------------------------------------------------

test_that("ds.omop.ohdsi.status has expected signature", {
  expect_true(is.function(ds.omop.ohdsi.status))
  args <- formals(ds.omop.ohdsi.status)
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_equal(args$symbol, "omop")
  expect_null(args$conns)
})

test_that("ds.omop.ohdsi.tables has expected signature", {
  expect_true(is.function(ds.omop.ohdsi.tables))
  args <- formals(ds.omop.ohdsi.tables)
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
})

test_that("ds.omop.ohdsi.tables never presents a partial catalog as pooled", {
  symbol <- "ohdsi_tables_partial"
  assign(
    symbol,
    structure(
      list(
        conns = list(a = "FAKE_A", b = "FAKE_B"),
        res_symbol = "dsO.ohdsi",
        server_names = c("a", "b")
      ),
      class = "omop_session"
    ),
    envir = dsOMOPClient:::.dsomop_client_env
  )
  on.exit(rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env),
          add = TRUE)

  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      server <- names(conns)[[1L]]
      if (identical(server, "b")) stop("offline", call. = FALSE)
      stats::setNames(list(data.frame(
        table_name = "cohort_count", tool_id = "cohort_diagnostics"
      )), server)
    },
    .package = "DSI"
  )

  result <- ds.omop.ohdsi.tables(symbol = symbol)
  expect_null(result$pooled)
  expect_named(attr(result$per_site, "ds_errors"), "b")
  expect_true(any(grepl("incomplete federation", result$meta$warnings)))
})

test_that("ds.omop.ohdsi.results has expected signature", {
  expect_true(is.function(ds.omop.ohdsi.results))
  args <- formals(ds.omop.ohdsi.results)
  expect_true("table_name" %in% names(args))
  expect_true("columns" %in% names(args))
  expect_true("filters" %in% names(args))
  expect_true("order_by" %in% names(args))
  expect_true("limit" %in% names(args))
  expect_true("tool_id" %in% names(args))
  expect_true("scope" %in% names(args))
  expect_true("pooling_policy" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
  expect_null(args$columns)
  expect_null(args$filters)
  expect_null(args$order_by)
  expect_null(args$tool_id)
})

test_that("ds.omop.ohdsi.summary has expected signature", {
  expect_true(is.function(ds.omop.ohdsi.summary))
  args <- formals(ds.omop.ohdsi.summary)
  expect_true("tool_id" %in% names(args))
  expect_true("symbol" %in% names(args))
  expect_true("conns" %in% names(args))
})

test_that("combined OHDSI results use the physical contract before shaping", {
  contract <- list(
    version = 1L,
    strategy = "tabular",
    columns = list(
      cohort_id = list(role = "key"),
      cohort_entries = list(role = "sum"),
      cohort_subjects = list(role = "sum")
    )
  )
  contract_meta <- list(
    contract_version = 1L,
    tool_id = "cohort_diagnostics",
    table_name = "cohort_count",
    pooling_contract = contract
  )
  per_site <- list(
    a = data.frame(cohort_id = c(1L, 2L),
                   cohort_entries = c(10, 20),
                   cohort_subjects = c(5, 10)),
    b = data.frame(cohort_id = c(1L, 2L),
                   cohort_entries = c(30, 40),
                   cohort_subjects = c(15, 20))
  )
  calls <- list()
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(a = "A", b = "B"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) {
      calls[[length(calls) + 1L]] <<- expr
      method <- as.character(expr[[1L]])
      if (identical(method, "omopOhdsiResultContractDS")) {
        return(list(a = contract_meta, b = contract_meta))
      }
      if (identical(method, "omopOhdsiResultsDS")) {
        return(lapply(per_site, function(frame) {
          frame[frame$cohort_id == 1L, , drop = FALSE]
        }))
      }
      stop("unexpected aggregate method", call. = FALSE)
    },
    .session_harmonization_for_connections = function(session, conns, ...) {
      list(poolable_counts = TRUE, count_band_width = 5L)
    },
    ds.omop.analysis.run = function(...) {
      stop("live analysis catalog must not be called")
    },
    .pool_result = function(...) {
      stop("heuristic pooler must not be called")
    },
    .package = "dsOMOPClient"
  )

  result <- ds.omop.ohdsi.results(
    "cohort_count", columns = c("cohort_id", "cohort_subjects"),
    filters = list(cohort_id = 1L), order_by = "cohort_subjects DESC",
    limit = 1L, tool_id = "cohort_diagnostics", type = "both"
  )
  expect_s3_class(result, "dsomop_result")
  expect_named(result$per_site, c("a", "b"))
  expect_identical(names(result$pooled),
                   c("cohort_id", "cohort_subjects"))
  expect_identical(result$pooled$cohort_id, 1L)
  expect_equal(result$pooled$cohort_subjects, 20)
  expect_identical(result$meta$tool_id, "cohort_diagnostics")
  expect_identical(result$meta$table_name, "cohort_count")

  expect_length(calls, 2L)
  expect_identical(as.character(calls[[1L]][[1L]]),
                   "omopOhdsiResultContractDS")
  results_call <- calls[[2L]]
  expect_identical(as.character(results_call[[1L]]), "omopOhdsiResultsDS")
  expect_null(results_call[[4L]])
  expect_identical(results_call[[5L]], .ds_encode(list(cohort_id = 1L)))
  expect_null(results_call[[6L]])
  expect_identical(results_call[[7L]], 5000L)
})

test_that("physical OHDSI contract mismatch fails before result retrieval", {
  contract <- list(version = 1L, strategy = "tabular", columns = list(
    cohort_id = list(role = "key"),
    cohort_subjects = list(role = "sum")
  ))
  meta_a <- list(contract_version = 1L, tool_id = "cohort_diagnostics",
                 table_name = "cohort_count", pooling_contract = contract)
  meta_b <- meta_a
  meta_b$table_name <- "different_table"
  results_requested <- FALSE
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(a = "A", b = "B"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) {
      if (identical(as.character(expr[[1L]]),
                    "omopOhdsiResultContractDS")) {
        return(list(a = meta_a, b = meta_b))
      }
      results_requested <<- TRUE
      stop("result endpoint must not be reached")
    },
    .package = "dsOMOPClient"
  )

  expect_error(
    ds.omop.ohdsi.results("cohort_count", type = "combine"),
    "differs across servers"
  )
  expect_false(results_requested)
})

test_that("combined OHDSI results fail closed at the server row cap", {
  contract <- list(version = 1L, strategy = "tabular", columns = list(
    cohort_id = list(role = "key"),
    cohort_subjects = list(role = "sum")
  ))
  contract_meta <- list(
    contract_version = 1L, tool_id = "cohort_diagnostics",
    table_name = "cohort_count", pooling_contract = contract
  )
  capped <- data.frame(cohort_id = seq_len(5000L), cohort_subjects = 5)
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(a = "A", b = "B"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) {
      if (identical(as.character(expr[[1L]]),
                    "omopOhdsiResultContractDS")) {
        return(list(a = contract_meta, b = contract_meta))
      }
      list(a = capped, b = capped)
    },
    .package = "dsOMOPClient"
  )

  expect_error(
    ds.omop.ohdsi.results("cohort_count", type = "combine"),
    "5000-row server cap"
  )
})

test_that("split OHDSI results preserve direct server-side shaping", {
  calls <- list()
  filters <- list(cohort_id = 1L)
  frame <- data.frame(cohort_id = 1L, cohort_subjects = 5)
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(a = "A"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) {
      calls[[length(calls) + 1L]] <<- expr
      list(a = frame)
    },
    .package = "dsOMOPClient"
  )

  result <- ds.omop.ohdsi.results(
    "cohort_count", columns = "cohort_subjects", filters = filters,
    order_by = "cohort_subjects DESC", limit = 7L,
    tool_id = "cohort_diagnostics", type = "split"
  )
  expect_s3_class(result, "dsomop_result")
  expect_identical(result$per_site$a, frame)
  expect_null(result$pooled)
  expect_length(calls, 1L)
  expect_identical(as.character(calls[[1L]][[1L]]), "omopOhdsiResultsDS")
  expect_identical(calls[[1L]][[4L]], "cohort_subjects")
  expect_identical(calls[[1L]][[5L]], .ds_encode(filters))
  expect_identical(calls[[1L]][[6L]],
                   .ds_encode_scalar("cohort_subjects DESC"))
  expect_identical(calls[[1L]][[7L]], 7L)

  opal_expression <- parse(
    text = paste(deparse(calls[[1L]]), collapse = "\n")
  )[[1L]]
  expect_identical(as.character(opal_expression[[6L]]),
                   .ds_encode_scalar("cohort_subjects DESC"))
})

test_that("split OHDSI ordering is validated before scalar encoding", {
  dispatched <- FALSE
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(a = "A"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(...) {
      dispatched <<- TRUE
      list()
    },
    .package = "dsOMOPClient"
  )

  invalid <- list(
    c("cohort_id", "cohort_subjects"), NA_character_, 1L,
    "", "cohort_id; DROP TABLE cohort"
  )
  for (value in invalid) {
    expect_error(
      ds.omop.ohdsi.results(
        "cohort_count", order_by = value, type = "split"
      ),
      "order_by must be one column"
    )
  }
  expect_false(dispatched)
})

test_that("split OHDSI columns are validated before dispatch", {
  dispatched <- FALSE
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(a = "A"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(...) {
      dispatched <<- TRUE
      list()
    },
    .package = "dsOMOPClient"
  )

  for (value in list(character(), NA_character_, c("cohort_id", NA), 1:2)) {
    expect_error(
      ds.omop.ohdsi.results(
        "cohort_count", columns = value, type = "split"
      ),
      "columns must be a non-empty character vector"
    )
  }
  expect_false(dispatched)
})

test_that("split OHDSI results project multi-column requests without Opal c", {
  calls <- list()
  requested_columns <- c("cohort_id", "cohort_subjects")
  frame <- data.frame(
    cohort_id = 1L, cohort_entries = 7,
    cohort_subjects = 5
  )
  testthat::local_mocked_bindings(
    .get_session = function(symbol) {
      list(conns = list(a = "A"), res_symbol = "omop_obj")
    },
    .ds_safe_aggregate = function(conns, expr) {
      calls[[length(calls) + 1L]] <<- expr
      list(a = frame)
    },
    .package = "dsOMOPClient"
  )

  result <- ds.omop.ohdsi.results(
    "cohort_count", columns = requested_columns,
    tool_id = "cohort_diagnostics", type = "split"
  )

  expect_identical(names(result$per_site$a), requested_columns)
  expect_length(calls, 1L)
  expect_null(calls[[1L]][[4L]])

  # Simulate Opal's deparse/parse transport. Before the fix, the fourth
  # argument reparsed as c("cohort_id", "cohort_subjects"), making Opal try
  # to resolve an aggregate method named `c`.
  opal_expression <- parse(
    text = paste(deparse(calls[[1L]]), collapse = "\n")
  )[[1L]]
  call_heads <- function(expression) {
    if (!is.call(expression)) return(character(0))
    c(
      as.character(expression[[1L]]),
      unlist(lapply(as.list(expression)[-1L], call_heads), use.names = FALSE)
    )
  }
  expect_false("c" %in% call_heads(opal_expression))
})

test_that("OHDSI result shaping is post-disclosure and deterministic", {
  value <- data.frame(
    cohort_id = c(2L, 1L, 1L),
    cohort_subjects = c(30, 20, 10),
    database_id = c("b", "a", "b")
  )
  shaped <- .ohdsi_shape_frame(
    value, columns = c("database_id", "cohort_subjects"),
    filters = list(cohort_id = 1L), order_by = "cohort_subjects DESC",
    limit = 1L
  )
  expect_identical(names(shaped), c("database_id", "cohort_subjects"))
  expect_identical(shaped$database_id, "a")
  expect_identical(shaped$cohort_subjects, 20)
})

test_that("generic OHDSI heuristic pooling is unavailable", {
  out <- .pool_result(
    list(a = data.frame(n_persons = 10),
         b = data.frame(n_persons = 20)),
    "ohdsi_results", "strict"
  )
  expect_null(out$result)
  expect_match(out$warnings, "Unknown result type")
})
