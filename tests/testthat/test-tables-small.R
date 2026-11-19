# dsOMOP 2.10.9: ds.omop.table() returns ds.table()'s object; small categories
# can be made missing for tables and summaries. Fixture: helper-concept-levels.R
# (with dsBase on the DSLite servers).

.tab_skip <- function() {
  testthat::skip_if_not_installed("dsBaseClient")
  testthat::skip_if_not_installed("dsBase")
}
# dsBase's levelsDS() also needs no more levels than a third of a site's rows.
.tab_deaths <- list(c(rep(910101L, 10L), 910102L, 910103L, rep(910104L, 4L), rep(910105L, 12L)),
                    rep(910001:910004, each = 5L), rep(910001:910004, each = 5L))

test_that("ds.omop.table returns ds.table's object with the published labels", {
  .tab_skip()
  ctx <- .cvl_setup(deaths = .tab_deaths)
  recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = FALSE)), out = c(study = "T"),
                 symbol = "cvl", conns = ctx$conns)
  # One site has two causes held by one person each: refused as in ds.table().
  refused <- ds.omop.table("T", "cause_of_death_first_value", conns = ctx$conns)
  expect_false(identical(refused$validity.message, "Data in all studies were valid"))
  tab <- ds.omop.table("T", "cause_of_death_first_value", small = "missing", conns = ctx$conns)
  expect_s3_class(tab, "dsomop_table")
  expect_identical(tab$validity.message, "Data in all studies were valid")
  combined <- tab$output.list$TABLES.COMBINED_all.sources_counts
  by_site <- tab$output.list$TABLE_rvar.by.study_counts
  expect_identical(colnames(by_site), names(ctx$conns))
  expect_equal(as.numeric(rowSums(by_site)), as.numeric(combined))
  # Secondary suppression: 910102 and 910103 (one person each) and the next
  # smallest, 910104 (four), are missing at site1; the others are exact.
  expect_equal(unname(by_site[c("910101", "910102", "910103", "910104", "910105"), "site1"]), c(10, 0, 0, 0, 12))
  expect_equal(as.numeric(combined[as.character(910001:910004)]), rep(10, 4))
  expect_output(print(tab), "By site")
})

test_that("drop_small_categories makes ds.summary and ds.table valid", {
  .tab_skip()
  ctx <- .cvl_setup(deaths = .tab_deaths)
  recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = FALSE)), out = c(study = "T"),
                 symbol = "cvl", conns = ctx$conns)
  # site1 has categories of one person; the small sites have more shared levels
  # than a third of their rows (dsBase's levelsDS() refuses).
  expect_error(dsBaseClient::ds.summary("T$cause_of_death_first_value", datasources = ctx$conns))
  ds.omop.drop_small_categories("T", newobj = "T_small", symbol = "cvl", conns = ctx$conns)
  after <- dsBaseClient::ds.summary("T_small$cause_of_death_first_value", datasources = ctx$conns)
  expect_true(all(vapply(after, is.list, logical(1))))
  plain <- dsBaseClient::ds.table("T_small$cause_of_death_first_value", datasources = ctx$conns)
  expect_identical(plain$validity.message, "Data in all studies were valid")
  # The published table is unchanged.
  expect_error(dsBaseClient::ds.summary("T$cause_of_death_first_value", datasources = ctx$conns))
})

test_that("two-way tables suppress small cells jointly", {
  .tab_skip()
  ctx <- .cvl_setup(deaths = .tab_deaths)
  variable <- function(name, reduce) omop_variable(event = .cvl_event, reducer = reduce, name = name)
  recipe <- omop_recipe(variables = list(cause = omop_variable(event = .cvl_event, reducer = .cvl_first),
                                         again = omop_variable(event = .cvl_event, reducer = omop_reduce("last_value", value = "cause_concept_id"))),
                        outputs = omop_output(name = "study", type = "wide"), options = list(translate_concepts = FALSE))
  recipe_execute(recipe, out = c(study = "W"), symbol = "cvl", conns = ctx$conns)
  tab <- ds.omop.table("W", "cause", by = "again", small = "missing", conns = ctx$conns)
  expect_identical(tab$validity.message, "Data in all studies were valid")
  counts <- tab$output.list$TABLES.COMBINED_all.sources_counts
  expect_identical(length(dim(counts)), 2L)
  expect_true(all(counts == 0 | counts >= 3))
})
