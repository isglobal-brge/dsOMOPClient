# dsOMOP 2.10.8: row verbs on a reconciled recipe output keep its shared
# factors instead of presenting them again (2.10.6 and 2.10.7 failed with
# "argument is of length zero" on every v2 output), and the identities of the
# rows they keep or stack, which later level reports read. Fixture:
# helper-concept-levels.R.

.verbs_report <- function(ctx, symbol, column) {
  reports <- DSI::datashield.aggregate(ctx$conns, call("omopFactorLevelsDS", as.symbol(symbol)))
  lapply(reports, function(report) list(unsafe = report$unsafe, levels = report$levels[[column]]))
}

test_that("filter and bind_rows work on reconciled recipe outputs and keep their levels", {
  ctx <- .cvl_setup()
  for (translate in c(TRUE, FALSE)) {
    source <- paste0("D_", translate)
    recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = translate)),
      out = c(study = source), symbol = "cvl", conns = ctx$conns)
    levels <- .cvl_shared(ctx, source, "cause_of_death_first_value")
    filtered <- paste0("F_", translate)
    ds.omop.filter(source, expr = omop_where(!is.na(cause_of_death_first_value)), newobj = filtered,
                   symbol = "cvl", conns = ctx$conns)
    expect_identical(.cvl_shared(ctx, filtered, "cause_of_death_first_value"), levels)
    stacked <- paste0("B_", translate)
    ds.omop.bind_rows(source, filtered, newobj = stacked, symbol = "cvl", conns = ctx$conns)
    expect_identical(.cvl_shared(ctx, stacked, "cause_of_death_first_value"), levels)
    sources <- .cvl_frames(ctx, source); bound <- .cvl_frames(ctx, stacked)
    for (site in names(sources)) {
      expect_identical(nrow(bound[[site]]), 2L * nrow(sources[[site]]))
      expect_identical(as.character(bound[[site]]$cause_of_death_first_value),
                       rep(as.character(sources[[site]]$cause_of_death_first_value), 2L))
    }
    # Level reports on the results read the kept and stacked rows.
    expected <- .verbs_report(ctx, source, "cause_of_death_first_value")
    expect_identical(.verbs_report(ctx, filtered, "cause_of_death_first_value"), expected)
    expect_identical(.verbs_report(ctx, stacked, "cause_of_death_first_value"), expected)
    model <- paste0("M_", translate)
    ds.omop.drop_empty_levels(stacked, newobj = model, symbol = "cvl", conns = ctx$conns)
    expect_true(all(as.character(c(sources$site1$cause_of_death_first_value, sources$site2$cause_of_death_first_value))
                    %in% .cvl_shared(ctx, model, "cause_of_death_first_value")))
  }
})
