# dsOMOP 2.10.9: published labels are dsBase-safe by default (letters, digits,
# . _ - +), identical at every site; the exact names stay in the codebook and
# options(dsomop.safe_labels = FALSE) keeps them. Fixture: helper-concept-levels.R.

test_that("published labels are dsBase-safe and identical at every site", {
  withr::local_options(list(dsomop.safe_labels = TRUE))
  ctx <- .cvl_setup()
  res <- recipe_execute(.cvl_wide(.cvl_event), out = c(study = "S"), symbol = "cvl", conns = ctx$conns)
  levels <- .cvl_shared(ctx, "S", "cause_of_death_first_value")
  expect_true(all(grepl("^[A-Za-z0-9._+-]+$", levels)))
  expect_true(all(paste0("Cause_", c(910001:910008, 910101:910160)) %in% levels))
  frames <- .cvl_frames(ctx, "S")
  expect_false(anyNA(frames$site1$cause_of_death_first_value))
  expect_identical(sort(unique(as.character(frames$site2$cause_of_death_first_value))), paste0("Cause_", 910001:910008))
  book <- recipe_codebook(res)[[1L]]$columns
  entry <- book$levels[[match("cause_of_death_first_value", book$name)]]
  expect_identical(entry$labels, levels)
  expect_true("Cause 910101" %in% entry$names)
  # Reports say how the column was published; model copies keep the labels.
  reports <- DSI::datashield.aggregate(ctx$conns, call("omopFactorLevelsDS", as.symbol("S")))
  expect_true(all(vapply(reports, function(r) "cause_of_death_first_value" %in% unlist(r$safe_columns), logical(1))))
  ds.omop.drop_empty_levels("S", newobj = "S_model", symbol = "cvl", conns = ctx$conns)
  expect_true(all(grepl("^[A-Za-z0-9._+-]+$", .cvl_shared(ctx, "S_model", "cause_of_death_first_value"))))
})

test_that("options(dsomop.safe_labels = FALSE) keeps the exact vocabulary names", {
  withr::local_options(list(dsomop.safe_labels = FALSE))
  ctx <- .cvl_setup()
  recipe_execute(.cvl_wide(.cvl_event), out = c(study = "N"), symbol = "cvl", conns = ctx$conns)
  expect_true("Cause 910101" %in% .cvl_shared(ctx, "N", "cause_of_death_first_value"))
})

test_that("staged outputs get the same safe labels", {
  withr::local_options(list(dsomop.safe_labels = TRUE))
  ctx <- .cvl_setup()
  recipe_execute(.cvl_wide(.cvl_event), out = c(study = "St"), symbol = "cvl", conns = ctx$conns, output_mode = "staged")
  recipe_execute(.cvl_wide(.cvl_event), out = c(study = "Me"), symbol = "cvl", conns = ctx$conns)
  expect_identical(.cvl_shared(ctx, "St", "cause_of_death_first_value"), .cvl_shared(ctx, "Me", "cause_of_death_first_value"))
  expect_identical(lapply(.cvl_frames(ctx, "St"), function(f) as.character(f$cause_of_death_first_value)),
                   lapply(.cvl_frames(ctx, "Me"), function(f) as.character(f$cause_of_death_first_value)))
})
