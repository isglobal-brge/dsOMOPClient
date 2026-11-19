# dsOMOP 2.10.9: recipe_execute(small_categories = "missing") publishes the
# outputs with categories held by fewer than nfilter.tab people missing at each
# site. Fixtures: helper-concept-levels.R, test-tables-small.R's deaths.

.tsr_deaths <- list(c(rep(910101L, 10L), 910102L, 910103L, rep(910104L, 4L), rep(910105L, 12L)),
                    rep(910001:910004, each = 5L), rep(910001:910004, each = 5L))

test_that("recipe_execute publishes with small categories missing when asked", {
  testthat::skip_if_not_installed("dsBaseClient")
  testthat::skip_if_not_installed("dsBase")
  ctx <- .cvl_setup(deaths = .tsr_deaths)
  recipe <- .cvl_wide(.cvl_event, options = list(translate_concepts = FALSE))
  expect_error(recipe_execute(recipe, out = c(study = "S"), symbol = "cvl", conns = ctx$conns,
                              output_mode = "staged", small_categories = "missing"), "in-memory")
  expect_false("S" %in% unlist(DSI::datashield.symbols(ctx$conns)))
  recipe_execute(recipe, out = c(study = "S"), symbol = "cvl", conns = ctx$conns, small_categories = "missing")
  plain <- dsBaseClient::ds.table("S$cause_of_death_first_value", datasources = ctx$conns)
  expect_identical(plain$validity.message, "Data in all studies were valid")
  # The output stays a publication with its contract. Secondary suppression at
  # site1: 910102 and 910103 (one person each), then 910104; no site holds
  # them now, so the table (of observed concepts) no longer lists them.
  tab <- ds.omop.table("S", "cause_of_death_first_value", conns = ctx$conns)
  expect_identical(tab$validity.message, "Data in all studies were valid")
  by_site <- tab$output.list$TABLE_rvar.by.study_counts
  expect_identical(sort(rownames(by_site)), as.character(sort(c(910001:910004, 910101L, 910105L))))
  expect_equal(unname(by_site[c("910101", "910105"), "site1"]), c(10, 12))
  # The same values as the table copy, and no temporary symbol is left.
  recipe_execute(recipe, out = c(study = "P"), symbol = "cvl", conns = ctx$conns)
  copy <- ds.omop.table("P", "cause_of_death_first_value", small = "missing", conns = ctx$conns)$output.list$TABLE_rvar.by.study_counts
  expect_equal(unname(copy[rownames(by_site), ]), unname(by_site))
  expect_true(all(copy[c("910102", "910103", "910104"), ] == 0))
  expect_false(any(grepl("^dsO", unlist(DSI::datashield.symbols(ctx$conns)))))
  # Copies of it work as for any publication.
  ds.omop.drop_small_categories("S", newobj = "S_small", symbol = "cvl", conns = ctx$conns)
  after <- dsBaseClient::ds.summary("S_small$cause_of_death_first_value", datasources = ctx$conns)
  expect_true(all(vapply(after, is.list, logical(1))))
  # ds.omop.table passes a contract-free copy's factor to ds.table() as it is.
  copied <- ds.omop.table("S_small", "cause_of_death_first_value", conns = ctx$conns)
  expect_s3_class(copied, "dsomop_table")
  expect_identical(copied$validity.message, "Data in all studies were valid")
  expect_error(ds.omop.table("S_small", "cause_of_death_first_value", small = "missing", conns = ctx$conns),
               "published concept columns")
})
