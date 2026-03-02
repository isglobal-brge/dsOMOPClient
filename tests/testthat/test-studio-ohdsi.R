# ==============================================================================
# Tests for OHDSI Results Studio module helper functions
# ==============================================================================

# --- .ohdsi_pick_result -------------------------------------------------------

test_that(".ohdsi_pick_result returns NULL for NULL input", {
  expect_null(.ohdsi_pick_result(NULL, "pooled", "srv_a"))
  expect_null(.ohdsi_pick_result(NULL, "per_site", "srv_a"))
})

test_that(".ohdsi_pick_result returns pooled when scope is pooled and pooled exists", {
  res <- list(
    per_site = list(srv_a = data.frame(x = 1)),
    pooled = data.frame(x = 10)
  )
  result <- .ohdsi_pick_result(res, "pooled", "srv_a")
  expect_equal(result$x, 10)
})

test_that(".ohdsi_pick_result falls back to per_site when pooled is NULL", {
  res <- list(
    per_site = list(srv_a = data.frame(x = 1)),
    pooled = NULL
  )
  result <- .ohdsi_pick_result(res, "pooled", "srv_a")
  expect_equal(result$x, 1)
})

test_that(".ohdsi_pick_result returns first server for per_site scope", {
  res <- list(
    per_site = list(
      srv_a = data.frame(x = 1),
      srv_b = data.frame(x = 2)
    ),
    pooled = NULL
  )
  result <- .ohdsi_pick_result(res, "per_site", "srv_b")
  expect_equal(result$x, 2)
})

test_that(".ohdsi_pick_result rbinds for scope all", {
  res <- list(
    per_site = list(
      srv_a = data.frame(x = 1),
      srv_b = data.frame(x = 2)
    ),
    pooled = NULL
  )
  result <- .ohdsi_pick_result(res, "all", NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("server" %in% names(result))
})

test_that(".ohdsi_pick_result handles empty per_site", {
  res <- list(per_site = list(), pooled = NULL)
  expect_null(.ohdsi_pick_result(res, "per_site", "srv_a"))
})

test_that(".ohdsi_pick_result handles empty data.frames in all scope", {
  res <- list(
    per_site = list(
      srv_a = data.frame(x = numeric(0)),
      srv_b = data.frame(x = 2)
    ),
    pooled = NULL
  )
  result <- .ohdsi_pick_result(res, "all", NULL)
  expect_equal(nrow(result), 1)
  expect_equal(result$x, 2)
})

# --- .ohdsi_dispatch_fetch ----------------------------------------------------

test_that(".ohdsi_dispatch_fetch returns no_data for unknown page", {
  state <- list(symbol = "omop")
  result <- .ohdsi_dispatch_fetch("Unknown Page", state, "per_site", "strict", NULL)
  expect_true(isTRUE(result$no_data))
})

# --- DQD pass/fail computation ------------------------------------------------

test_that("DQD failed column computation handles all-NA threshold", {
  df <- data.frame(
    check_name = c("a", "b", "c"),
    category = c("Completeness", "Conformance", "Plausibility"),
    num_violated_rows = c(5L, 0L, 10L),
    num_denominator_rows = c(100L, 50L, 200L),
    threshold_value = c(NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )

  # Simulate the fetch logic (with NA guards)
  viol <- as.numeric(df$num_violated_rows)
  denom <- as.numeric(df$num_denominator_rows)
  thresh <- ifelse(is.na(as.numeric(df$threshold_value)), 0,
                   as.numeric(df$threshold_value))
  failed <- ifelse(is.na(denom) | denom == 0, FALSE,
                   ifelse(is.na(viol), FALSE, (viol / denom) > thresh))

  expect_equal(failed, c(TRUE, FALSE, TRUE))
  expect_false(any(is.na(failed)))
})

test_that("DQD failed column computation handles zero denominator", {
  df <- data.frame(
    num_violated_rows = c(5L, 0L),
    num_denominator_rows = c(0L, 0L),
    stringsAsFactors = FALSE
  )

  viol <- as.numeric(df$num_violated_rows)
  denom <- as.numeric(df$num_denominator_rows)
  failed <- ifelse(is.na(denom) | denom == 0, FALSE,
                   ifelse(is.na(viol), FALSE, (viol / denom) > 0))

  expect_equal(failed, c(FALSE, FALSE))
})

test_that("DQD failed column computation handles NA in violated rows", {
  df <- data.frame(
    num_violated_rows = c(NA_integer_, 5L),
    num_denominator_rows = c(100L, 100L),
    stringsAsFactors = FALSE
  )

  viol <- as.numeric(df$num_violated_rows)
  denom <- as.numeric(df$num_denominator_rows)
  failed <- ifelse(is.na(denom) | denom == 0, FALSE,
                   ifelse(is.na(viol), FALSE, (viol / denom) > 0))

  expect_equal(failed, c(FALSE, TRUE))
})

# --- .ohdsi_render_overview ---------------------------------------------------

test_that(".ohdsi_render_overview produces UI with tool cards", {
  ns <- shiny::NS("test")
  data <- list(
    tools_summary = list(
      dqd = list(
        name = "Data Quality Dashboard", icon = "clipboard-check",
        theme = "success", available = TRUE,
        per_server = list(srv_a = TRUE), n_tables = 1L, total_rows = 100L
      ),
      cohort_diagnostics = list(
        name = "CohortDiagnostics", icon = "users",
        theme = "primary", available = FALSE,
        per_server = list(srv_a = FALSE), n_tables = 0L, total_rows = 0L
      ),
      cohort_incidence = list(
        name = "CohortIncidence", icon = "chart-line",
        theme = "info", available = FALSE,
        per_server = list(srv_a = FALSE), n_tables = 0L, total_rows = 0L
      ),
      characterization = list(
        name = "Characterization", icon = "microscope",
        theme = "warning", available = FALSE,
        per_server = list(srv_a = FALSE), n_tables = 0L, total_rows = 0L
      )
    ),
    tables_res = NULL
  )

  ui <- .ohdsi_render_overview(ns, data)
  html <- as.character(ui)
  expect_true(grepl("Data Quality Dashboard", html))
  expect_true(grepl("Available", html))
  expect_true(grepl("Not Found", html))
  expect_true(grepl("srv_a", html))
})

# --- .ohdsi_render_dqd --------------------------------------------------------

test_that(".ohdsi_render_dqd shows empty tool state when no_data", {
  ns <- shiny::NS("test")
  data <- list(no_data = TRUE, tool_missing = "dqd")

  ui <- .ohdsi_render_dqd(ns, data)
  html <- as.character(ui)
  expect_true(grepl("Data Quality Dashboard", html))
  expect_true(grepl("Not Found", html))
})

test_that(".ohdsi_render_dqd renders KPIs and chart/table containers", {
  ns <- shiny::NS("test")
  data <- list(
    results = data.frame(
      check_name = c("a", "b"), check_type = c("Completeness", "Conformance"),
      failed = c(FALSE, TRUE), num_violated_rows = c(0, 5),
      num_denominator_rows = c(100, 50),
      stringsAsFactors = FALSE
    ),
    total_checks = 2, n_pass = 1, pass_rate = 50.0, n_categories = 2
  )

  ui <- .ohdsi_render_dqd(ns, data)
  html <- as.character(ui)
  expect_true(grepl("Pass Rate", html))
  expect_true(grepl("50%", html))
  expect_true(grepl("Total Checks", html))
  expect_true(grepl("dqd_category_plot", html))
  expect_true(grepl("dqd_results_table", html))
})

# --- .ohdsi_render_cohort_counts ----------------------------------------------

test_that(".ohdsi_render_cohort_counts shows empty tool state when no_data", {
  ns <- shiny::NS("test")
  data <- list(no_data = TRUE, tool_missing = "cohort_diagnostics")

  ui <- .ohdsi_render_cohort_counts(ns, data)
  html <- as.character(ui)
  expect_true(grepl("CohortDiagnostics", html))
  expect_true(grepl("Not Found", html))
})

test_that(".ohdsi_render_cohort_counts renders KPIs and chart/table", {
  ns <- shiny::NS("test")
  data <- list(
    cohort_counts = data.frame(
      cohort_id = c(1, 2), cohort_subjects = c(100, 50),
      stringsAsFactors = FALSE
    ),
    total_subjects = 150, n_cohorts = 2
  )

  ui <- .ohdsi_render_cohort_counts(ns, data)
  html <- as.character(ui)
  expect_true(grepl("Cohorts", html))
  expect_true(grepl("Total Subjects", html))
  expect_true(grepl("cohort_bar_plot", html))
  expect_true(grepl("cohort_results_table", html))
})

# --- .ohdsi_render_incidence --------------------------------------------------

test_that(".ohdsi_render_incidence shows empty tool state when no_data", {
  ns <- shiny::NS("test")
  data <- list(no_data = TRUE, tool_missing = "cohort_incidence")

  ui <- .ohdsi_render_incidence(ns, data)
  html <- as.character(ui)
  expect_true(grepl("CohortIncidence", html))
  expect_true(grepl("Not Found", html))
})

test_that(".ohdsi_render_incidence renders KPIs and chart/table", {
  ns <- shiny::NS("test")
  data <- list(
    incidence = data.frame(
      outcome_id = 1, incidence_rate = 15.0,
      stringsAsFactors = FALSE
    ),
    n_records = 1
  )

  ui <- .ohdsi_render_incidence(ns, data)
  html <- as.character(ui)
  expect_true(grepl("Records", html))
  expect_true(grepl("incidence_bar_plot", html))
  expect_true(grepl("incidence_results_table", html))
})

# --- .ohdsi_render_characterization -------------------------------------------

test_that(".ohdsi_render_characterization shows empty tool state when no_data", {
  ns <- shiny::NS("test")
  data <- list(no_data = TRUE, tool_missing = "characterization")

  ui <- .ohdsi_render_characterization(ns, data)
  html <- as.character(ui)
  expect_true(grepl("Characterization", html))
  expect_true(grepl("Not Found", html))
})

test_that(".ohdsi_render_characterization renders KPIs and chart/table", {
  ns <- shiny::NS("test")
  data <- list(
    cohort_counts = data.frame(cohort_id = 1, num_persons = 200,
                                stringsAsFactors = FALSE),
    covariates = data.frame(
      covariate_id = c(1, 2),
      covariate_name = c("Age group", "Gender"),
      average_value = c(0.45, 0.55),
      stringsAsFactors = FALSE
    ),
    n_cohorts = 1, n_covariates = 2
  )

  ui <- .ohdsi_render_characterization(ns, data)
  html <- as.character(ui)
  expect_true(grepl("Cohorts", html))
  expect_true(grepl("Covariates", html))
  expect_true(grepl("char_covariate_plot", html))
  expect_true(grepl("char_results_table", html))
})

# --- .ohdsi_empty_tool --------------------------------------------------------

test_that(".ohdsi_empty_tool renders with tool name and message", {
  ui <- .ohdsi_empty_tool("TestTool", "Install TestTool first")
  html <- as.character(ui)
  expect_true(grepl("TestTool", html))
  expect_true(grepl("Install TestTool first", html))
  expect_true(grepl("empty-state", html))
})

# --- .ohdsi_accumulate_code ---------------------------------------------------

test_that(".ohdsi_accumulate_code appends code for dsomop_result", {
  state <- shiny::reactiveValues(script_lines = character(0))
  res <- structure(
    list(
      per_site = list(), pooled = NULL,
      meta = list(call_code = "ds.omop.ohdsi.status()")
    ),
    class = "dsomop_result"
  )
  shiny::isolate(.ohdsi_accumulate_code(state, res))
  shiny::isolate(expect_equal(state$script_lines, "ds.omop.ohdsi.status()"))
})

test_that(".ohdsi_accumulate_code ignores non-dsomop_result", {
  state <- shiny::reactiveValues(script_lines = character(0))
  .ohdsi_accumulate_code(state, list(per_site = list()))
  shiny::isolate(expect_equal(length(state$script_lines), 0))
})

test_that(".ohdsi_accumulate_code ignores empty call_code", {
  state <- shiny::reactiveValues(script_lines = character(0))
  res <- structure(
    list(per_site = list(), pooled = NULL,
         meta = list(call_code = "")),
    class = "dsomop_result"
  )
  shiny::isolate(.ohdsi_accumulate_code(state, res))
  shiny::isolate(expect_equal(length(state$script_lines), 0))
})

# --- Multi-server edge cases --------------------------------------------------

test_that(".ohdsi_pick_result handles mismatched server names", {
  res <- list(
    per_site = list(srv_a = data.frame(x = 1)),
    pooled = NULL
  )
  # Request a server that doesn't exist
  result <- .ohdsi_pick_result(res, "per_site", "nonexistent")
  # .resolve_servers returns all_srvs when intersection is empty
  expect_null(result)
})

test_that(".ohdsi_pick_result handles character(0) server", {
  res <- list(
    per_site = list(srv_a = data.frame(x = 1)),
    pooled = NULL
  )
  result <- .ohdsi_pick_result(res, "per_site", character(0))
  # character(0) is handled by .resolve_servers (length == 0 -> all servers)
  expect_equal(result$x, 1)
})
