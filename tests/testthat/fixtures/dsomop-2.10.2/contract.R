# The argument contract of dsOMOP 2.10.2 (tag 2.10.2, server commit 30b7905),
# extracted verbatim for the legacy-server regressions of WS-8 (review B1):
#   .omopAnalysisScopeFromDots  R/interface.R:2621 (verbatim)
#   omopPlanExecuteDS            R/interface.R:713 (signature only)
#   omopDpPlanExecuteDS          R/privacy.R:955 (signature only)
# Both execution methods pass every argument they do not name to
# .omopAnalysisScopeFromDots(scope, list(...)), which accepts only the
# scope_cohort_N / scope_table_N sources.

.omopAnalysisScopeFromDots <- function(scope, dots) {
  if (!is.list(dots)) {
    stop("Internal error: analysis scope dots must be a list.", call. = FALSE)
  }
  server_max <- as.integer(
    .omopDisclosureSettings()$max_analysis_scope_tables
  )
  total_max <- server_max + 1L
  .omopAnalysisScopeSourceCount(scope, max_sources = total_max)
  scope <- .omopAnalysisNormalizePublicScope(scope)
  if (length(dots) == 0L) return(scope)

  if (length(dots) > total_max) {
    stop("Analysis scope exceeds the server total source cap of ", total_max,
         ".", call. = FALSE)
  }

  dot_names <- names(dots)
  if (is.null(dot_names) || anyNA(dot_names) || any(!nzchar(dot_names))) {
    stop("Additional analysis scope sources must be strictly named ",
         "scope_cohort_1, ... or scope_table_1, ...", call. = FALSE)
  }
  if (anyDuplicated(dot_names)) {
    stop("Additional analysis scope source names must be unique.", call. = FALSE)
  }
  if (any(!grepl("^scope_(cohort|table)_[1-9][0-9]*$", dot_names))) {
    stop("Unexpected analysis argument name; additional scope sources must be ",
         "named scope_cohort_N or scope_table_N.", call. = FALSE)
  }

  ordered_family <- function(family) {
    prefix <- paste0("scope_", family, "_")
    selected <- startsWith(dot_names, prefix)
    values <- dots[selected]
    if (length(values) == 0L) return(list())
    indices <- suppressWarnings(as.integer(sub(prefix, "", names(values),
                                               fixed = TRUE)))
    if (anyNA(indices) ||
        !identical(sort(indices), seq_len(length(indices)))) {
      stop("Additional analysis scope ", family,
           " names must be contiguous from ", prefix,
           "1 with no gaps.", call. = FALSE)
    }
    unname(values[order(indices)])
  }

  cohort_dots <- ordered_family("cohort")
  table_dots <- ordered_family("table")
  if (any(!vapply(table_dots, .is_omop.table, logical(1)))) {
    stop("Each scope_table_N argument must resolve to an omop.table workspace ",
         "object.", call. = FALSE)
  }
  cohort_dots <- lapply(cohort_dots, .omopAnalysisCohortScopeScalar)

  scope_sources <- if (is.null(scope)) {
    list()
  } else if (is.list(scope) && !is.data.frame(scope) &&
             !.is_omop.table(scope)) {
    unname(scope)
  } else {
    list(scope)
  }
  scope_is_table <- vapply(scope_sources, .is_omop.table, logical(1))
  combined <- c(scope_sources[!scope_is_table], cohort_dots,
                scope_sources[scope_is_table], table_dots)
  .omopAnalysisScopeSourceCount(combined, max_sources = total_max)
  if (.omopAnalysisScopeTableCount(combined) > server_max) {
    stop("Analysis scope exceeds the server max_analysis_scope_tables cap of ",
         server_max, ".", call. = FALSE)
  }
  if (length(combined) == 1L) combined[[1L]] else unname(combined)
}

.omopPlanExecuteDS_2.10.2 <- function(omop_symbol, plan, out,
                               output_mode = "memory",
                               scope = NULL, combine = "union", ...) NULL

.omopDpPlanExecuteDS_2.10.2 <- function(omop_symbol, plan, out,
                                output_mode = "memory",
                                scope = NULL, combine = "union", ...) NULL
