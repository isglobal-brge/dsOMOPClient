# dsOMOP 2.10.7: ds.omop.drop_empty_levels() makes a copy for models whose
# factor columns keep only levels that hold a value at some site. Each site
# reports its levels in use under DataSHIELD's level rule. Fixture:
# helper-concept-levels.R.

.ml_causes <- function(ctx) omop_concept_levels(omop_concept_set(.cvl_root, descendants = TRUE),
                                               symbol = "cvl", conns = ctx$conns)

test_that("levels empty at every site are dropped identically, values unchanged", {
  # site1: 30 causes in use; sites 2 and 3: eight. 69 declared levels.
  ctx <- .cvl_setup(deaths = list(rep(910101:910130, each = 4L), rep(910001:910008, each = 5L),
                                  rep(910001:910008, each = 5L)))
  recipe_execute(.cvl_wide(.cvl_event, omop_category_spec(levels = .ml_causes(ctx)),
      options = list(translate_concepts = FALSE)),
    out = c(study = "D"), symbol = "cvl", conns = ctx$conns)
  expect_length(.cvl_shared(ctx, "D", "cause_of_death_first_value"), 69L)
  summary <- ds.omop.drop_empty_levels("D", newobj = "D_model", symbol = "cvl", conns = ctx$conns)
  row <- summary[summary$column == "cause_of_death_first_value", ]
  expect_identical(c(row$levels, row$kept), c(69L, 38L))
  expect_match(row$status, "^dropped levels empty at every site")
  # The declared reference (the root concept, first in ID order) is held by
  # nobody: the summary says the first kept category becomes the reference.
  expect_match(row$status, "the reference '910000' was one of them", fixed = TRUE)
  kept <- .cvl_shared(ctx, "D_model", "cause_of_death_first_value")
  expect_setequal(kept, as.character(c(910001:910008, 910101:910130)))
  original <- .cvl_frames(ctx, "D")
  model <- .cvl_frames(ctx, "D_model")
  for (site in names(model)) {
    f <- model[[site]]$cause_of_death_first_value
    expect_identical(as.character(f), as.character(original[[site]]$cause_of_death_first_value))
    # Shared levels stay in model frames, so every site has the same design.
    expect_identical(ncol(stats::model.matrix(~ cause_of_death_first_value, model[[site]])), 38L)
    expect_length(levels(original[[site]]$cause_of_death_first_value), 69L)
  }
})

test_that("a column whose levels in use fail the rule at a site keeps all its levels", {
  ctx <- .cvl_setup()
  recipe_execute(.cvl_wide(.cvl_event, omop_category_spec(levels = .ml_causes(ctx)),
      options = list(translate_concepts = FALSE)),
    out = c(study = "D"), symbol = "cvl", conns = ctx$conns)
  expect_message(summary <- ds.omop.drop_empty_levels("D", symbol = "cvl", conns = ctx$conns),
                 "keep all their levels")
  row <- summary[summary$column == "cause_of_death_first_value", ]
  expect_identical(row$kept, 69L)
  expect_match(row$status, "site1", fixed = TRUE)
  expect_false(grepl("site2|site3", row$status))
  expect_length(.cvl_shared(ctx, "D_model", "cause_of_death_first_value"), 69L)
})

test_that("calendars and categories keep their class, order and contrasts", {
  ctx <- .cvl_setup(deaths = list(rep(910101:910112, each = 4L), rep(910001:910008, each = 5L),
                                  rep(910001:910008, each = 5L)))
  for (path in ctx$paths) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbExecute(con, "UPDATE death SET death_date = date('2020-01-01', '+' || ((person_id * 7) % 360) || ' days')")
    DBI::dbDisconnect(con)
  }
  causes <- .ml_causes(ctx)
  recipe <- omop_recipe(variables = list(
      died = omop_date("death", omop_date_spec(precision = "month", level_range = c("2015-01-01", "2024-12-31"))),
      cause = omop_category(omop_variable(event = .cvl_event, reducer = .cvl_first),
        omop_category_spec(levels = causes, reference = "concept:910000"))),
    outputs = omop_output(name = "study", type = "wide"), options = list(translate_concepts = FALSE))
  recipe_execute(recipe, out = c(study = "D"), symbol = "cvl", conns = ctx$conns)
  ds.omop.drop_empty_levels("D", newobj = "D_model", symbol = "cvl", conns = ctx$conns)
  months <- .cvl_shared(ctx, "D_model", "died")
  expect_true(length(months) <= 12L && all(startsWith(months, "2020-")))
  expect_identical(months, sort(months))
  cause <- .cvl_shared(ctx, "D_model", "cause")
  expect_length(cause, 20L)
  for (frame in .cvl_frames(ctx, "D_model")) {
    expect_s3_class(frame$died, "dsomop_date_factor")
    expect_true(is.ordered(frame$died))
    expect_s3_class(frame$cause, "dsomop_category_factor")
    # The declared reference held no value anywhere: the first kept level is.
    expect_identical(dim(attr(frame$cause, "contrasts")), c(20L, 19L))
    expect_identical(unname(which(rowSums(attr(frame$cause, "contrasts")) == 0)), 1L)
    expect_identical(ncol(stats::model.matrix(~ died + cause, frame)), 1L + length(months) - 1L + 19L)
  }
})

test_that("only published factor columns of an output are reported or relevelled", {
  ctx <- .cvl_setup()
  recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = FALSE)),
    out = c(study = "D"), symbol = "cvl", conns = ctx$conns)
  expect_error(ds.omop.drop_empty_levels("D", columns = "person_id", symbol = "cvl", conns = ctx$conns),
               "could not be read")
  expect_error(ds.omop.drop_empty_levels("D", columns = c("a", "a"), symbol = "cvl", conns = ctx$conns),
               "distinct column names")
})
