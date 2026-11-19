# Declared concept levels from the public vocabulary need no discovery and have
# no factor-level limit; undeclared observed levels keep DataSHIELD's rule,
# with each site's own options. Fixture: helper-concept-levels.R.

test_that("undeclared observed causes over the cap refuse with every remedy and revoke all sites", {
  # The 2.10.7 automatic vocabulary list is off on these servers.
  ctx <- .cvl_setup(auto_max = "0")
  for (translate in c(TRUE, FALSE)) {
    error <- tryCatch({
      recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = translate)),
        out = c(study = "Refused"), symbol = "cvl", conns = ctx$conns)
      NULL
    }, error = identity)
    expect_s3_class(error, "error")
    message <- conditionMessage(error)
    for (part in c("site1", "nfilter.levels.max", "omop_concept_levels()", "omop_category_recode()",
                   "concepts =", "administrator")) expect_match(message, part, fixed = TRUE)
    expect_false(grepl("site2|site3", message))
    expect_false(grepl("[0-9]+ (levels|rows|persons)", message))
    expect_true(all(vapply(.cvl_frames(ctx, "Refused"), is.null, logical(1))))
  }
})

test_that("each server's raised nfilter.levels.max admits more observed levels", {
  ctx <- .cvl_setup(levels_max = "100")
  recipe_execute(.cvl_wide(.cvl_event), out = c(study = "Raised"), symbol = "cvl", conns = ctx$conns)
  expect_length(.cvl_shared(ctx, "Raised", "cause_of_death_first_value"), 68L)
})

test_that("omop_concept_levels declares vocabulary levels with no discovery or level limit", {
  ctx <- .cvl_setup()
  keys <- omop_concept_levels(omop_concept_set(.cvl_root, descendants = TRUE), symbol = "cvl", conns = ctx$conns)
  expect_identical(keys, paste0("concept:", sort(c(.cvl_root, .cvl_causes))))
  for (translate in c(TRUE, FALSE)) {
    target <- paste0("Declared_", translate)
    recipe_execute(.cvl_wide(.cvl_event, omop_category_spec(levels = keys),
      options = list(translate_concepts = translate)), out = c(study = target), symbol = "cvl", conns = ctx$conns)
    levels <- .cvl_shared(ctx, target, "cause_of_death_first_value")
    expect_length(levels, length(keys))
    if (!translate) expect_setequal(levels, sub("^concept:", "", keys))
    .cvl_reports_identical(ctx, target)
  }
  excluded <- omop_concept_levels(omop_concept_set(.cvl_root, descendants = TRUE, exclude = .cvl_root),
    symbol = "cvl", conns = ctx$conns)
  expect_false(paste0("concept:", .cvl_root) %in% excluded)
  expect_error(omop_concept_levels(.cvl_causes, symbol = "cvl", conns = ctx$conns), "omop_concept_set")
})

test_that("values outside a declared list are discovered under the rule and join the shared levels", {
  ctx <- .cvl_setup()
  keys <- omop_concept_levels(omop_concept_set(.cvl_root, descendants = TRUE, exclude = 910101L),
    symbol = "cvl", conns = ctx$conns)
  recipe_execute(.cvl_wide(.cvl_event, omop_category_spec(levels = keys), options = list(translate_concepts = FALSE)),
    out = c(study = "Outside"), symbol = "cvl", conns = ctx$conns)
  expect_true("910101" %in% .cvl_shared(ctx, "Outside", "cause_of_death_first_value"))
})

test_that("an event restricted by a concept set declares its resolved set automatically", {
  ctx <- .cvl_setup()
  for (translate in c(TRUE, FALSE)) {
    target <- paste0("Event_", translate)
    recipe_execute(.cvl_wide(.cvl_set_event, options = list(translate_concepts = translate)),
      out = c(study = target), symbol = "cvl", conns = ctx$conns)
    expect_length(.cvl_shared(ctx, target, "cause_of_death_first_value"), length(.cvl_causes) + 1L)
    .cvl_reports_identical(ctx, target)
  }
  recipe_execute(omop_recipe(events = list(.cvl_set_event),
      outputs = omop_output(name = "events", format = "long", sources = list(.cvl_set_event))),
    out = c(events = "Long"), symbol = "cvl", conns = ctx$conns)
  expect_length(.cvl_shared(ctx, "Long", "concept_id"), length(.cvl_causes) + 1L)
  .cvl_reports_identical(ctx, "Long")
})

test_that("a hierarchy rollup over a vocabulary concept set needs no discovery", {
  ctx <- .cvl_setup()
  spec <- omop_category_spec(rollup = omop_category_rollup("ancestors",
    ancestors = paste0("concept:", .cvl_root), vocabulary = "Fixture",
    domain = omop_concept_set(.cvl_root, descendants = TRUE)))
  recipe_execute(.cvl_wide(.cvl_event, spec), out = c(study = "Rollup"), symbol = "cvl", conns = ctx$conns)
  expect_length(.cvl_shared(ctx, "Rollup", "cause_of_death_first_value"), 1L)
  .cvl_reports_identical(ctx, "Rollup")
})
