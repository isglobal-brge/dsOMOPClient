for (cause in c("no_drugs", "eligibility")) local({
  empty_cause <- cause
  test_that(paste("empty comparator site reaches support policy after", empty_cause), {
    ctx <- .library_cdm_setup(two_sites = TRUE, second_site = "unchanged")
    session <- dsOMOPClient:::.get_session(ctx$symbol)
    server <- ctx$servers$site2
    handle <- server$getSessionData(server$getSessionIds()[[1L]],
      paste0(".dsomop_handle_", session$res_symbol))
    if (empty_cause == "no_drugs") {
      DBI::dbExecute(handle$conn, "DELETE FROM drug_exposure")
    } else {
      # Retain all drug candidates but remove the required 365-day observed
      # history at every candidate date. The ordinary template stays unchanged.
      DBI::dbExecute(handle$conn,
        "UPDATE observation_period SET observation_period_start_date = '2020-01-01'")
      expect_equal(DBI::dbGetQuery(handle$conn,
        "SELECT COUNT(*) AS n FROM drug_exposure")$n, 72L)
    }
    r <- .library_test_recipe("cohort.active_comparator")
    retained <- list()
    for (mode in c("memory", "staged")) {
      out <- setNames(paste0("D_empty_comparator_", mode, "_", r$publish), r$publish)
      before <- DSI::datashield.symbols(ctx$conns)
      staged_files <- function() sort(list.files(getOption("dsstaging.base_dir"),
        recursive = TRUE, all.files = TRUE, full.names = TRUE))
      files_before <- staged_files()
      # Omit on_small_site to exercise the public default, including rollback
      # of both roots already prepared at the adequately supported first site.
      expect_error(recipe_execute(r, out = out, symbol = ctx$symbol,
        conns = ctx$conns, output_mode = mode), "site2:.*insufficient_support")
      expect_identical(DSI::datashield.symbols(ctx$conns), before)
      expect_identical(staged_files(), files_before)

      result <- recipe_execute(r, out = out, symbol = ctx$symbol,
        conns = ctx$conns, output_mode = mode, on_small_site = "skip")
      expect_null(result$site2)
      expect_false(is.null(result$site1))
      after <- DSI::datashield.symbols(ctx$conns)
      expect_identical(after$site2, before$site2)
      expect_setequal(setdiff(after$site1, before$site1), unname(out))
      supported <- ctx
      supported$servers <- ctx$servers["site1"]
      frames <- lapply(out, function(symbol) .library_frames(supported, symbol)$site1)
      .library_assert_values("cohort.active_comparator", frames)
      .library_compare_oracle("cohort.active_comparator", frames)
      for (root in names(frames)) {
        expect_identical(names(frames[[root]]),
          .library_expected_columns("cohort.active_comparator", root, r))
        expect_equal(length(unique(frames[[root]]$person_id)), 24L)
        if (mode == "staged") {
          for (column in names(frames[[root]])) {
            expect_identical(as.character(frames[[root]][[column]]),
              as.character(retained[[root]][[column]]))
          }
        }
      }
      if (mode == "memory") retained <- frames
    }
  })
})
