# dsOMOP 2.10.8: row verbs on a reconciled recipe output keep its shared
# factors instead of presenting them again (2.10.6 and 2.10.7 failed with
# "argument is of length zero" on every v2 output). Fixture:
# helper-concept-levels.R.

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
  }
})
