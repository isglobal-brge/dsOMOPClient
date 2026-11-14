# A-31 Q04/Q05: these selectors were advertised but did not affect selection.
.audit_cases <- list(
  list(id = "dsomop:incidence.rate", parameter = "clean_window", reason = "unsupported_clean_window"),
  list(id = "dsomop:ohdsi.cohort_diagnostics.incidence_rate", parameter = "clean_window", reason = "unsupported_clean_window"),
  list(id = "dsomop:ohdsi.cohort_incidence.incidence_summary", parameter = "clean_window", reason = "unsupported_clean_window"),
  list(id = "dsomop:fe.prevalence", parameter = "time_windows", reason = "unsupported_fe_window"))

test_that("effective audit settings are strict and normalize only supported defaults", {
  for (case in .audit_cases) {
    omitted <- .auditEffectiveSettings(case$id, list())
    for (zero in list(0L, 0, "0", "+0", "-0", "0.0", "0e0")) {
      explicit <- .auditEffectiveSettings(case$id, setNames(list(zero), case$parameter))
      expect_identical(explicit, omitted)
    }
    for (value in list(1, -1, "365")) {
      expect_error(.auditEffectiveSettings(case$id, setNames(list(value), case$parameter)), case$reason)
    }
    for (value in list(NULL, NA, NaN, Inf, .5, "0.5", "NaN", "Inf", "secret-sql", TRUE,
                       integer(0), c(0, 1), matrix(0), list(0), structure(0, class = "opaque"))) {
      expect_error(.auditEffectiveSettings(case$id, setNames(list(value), case$parameter)), "invalid_setting")
    }
  }
})

test_that("client execution and generated calls refuse before a session or network is needed", {
  for (case in .audit_cases) {
    params <- setNames(list(1), case$parameter)
    expect_error(ds.omop.analysis.run(case$id, params = params), case$reason)
    expect_error(.analysis_run_call("omopAnalysisRunDS", "omop", case$id, params,
                                   NULL, "union"), case$reason)
    omitted <- .analysis_run_call("omopAnalysisRunDS", "omop", case$id, list(), NULL, "union")
    explicit <- .analysis_run_call("omopAnalysisRunDS", "omop", case$id,
                                   setNames(list(0), case$parameter), NULL, "union")
    expect_identical(omitted, explicit)
  }
})
