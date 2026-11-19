# dsOMOP 2.10.7: a concept column whose observed levels fail DataSHIELD's
# rule declares, on its own, the short public vocabulary list that holds them;
# calendars declare every period of their observed years. Fixture:
# helper-concept-levels.R (site1: 60 of 68 causes; sites 2 and 3: eight).

.cvl_vocabulary <- sort(c(.cvl_root, .cvl_causes))

test_that("observed causes over the cap publish with their vocabulary list declared", {
  ctx <- .cvl_setup()
  for (mode in c("memory", "staged")) for (translate in c(TRUE, FALSE)) {
    target <- paste0("Auto_", mode, "_", translate)
    recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = translate)),
      out = c(study = target), symbol = "cvl", conns = ctx$conns, output_mode = mode)
    levels <- .cvl_shared(ctx, target, "cause_of_death_first_value")
    expect_length(levels, length(.cvl_vocabulary))
    if (!translate) expect_setequal(levels, as.character(.cvl_vocabulary))
    # Each site still holds its own rows: only the level set is shared.
    frames <- .cvl_frames(ctx, target)
    expect_identical(nrow(frames$site1), 240L)
    expect_length(unique(stats::na.omit(as.character(frames$site2$cause_of_death_first_value))), 8L)
  }
  reports <- DSI::datashield.aggregate(ctx$conns, call("omopFactorLevelsDS", as.symbol("Auto_memory_FALSE")))
  for (report in reports) expect_length(report$unsafe, 0L)
})

test_that("columns whose observed levels pass the rule keep exactly those levels", {
  eight <- rep(910001:910008, each = 5L)
  ctx <- .cvl_setup(deaths = list(eight, eight, eight))
  recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = FALSE)),
    out = c(study = "Passing"), symbol = "cvl", conns = ctx$conns)
  expect_setequal(.cvl_shared(ctx, "Passing", "cause_of_death_first_value"), as.character(910001:910008))
})

test_that("a vocabulary list longer than the server option stays refused and says so", {
  ctx <- .cvl_setup(auto_max = "10")
  error <- tryCatch({
    recipe_execute(.cvl_wide(.cvl_event), out = c(study = "Long"), symbol = "cvl", conns = ctx$conns)
    NULL
  }, error = identity)
  expect_s3_class(error, "error")
  message <- conditionMessage(error)
  for (part in c("site1", "dsomop.auto_declared_levels_max", "longer than the server declares automatically",
                 "omop_concept_levels()", "omop_category_rollup()", "administrator"))
    expect_match(message, part, fixed = TRUE)
  expect_false(grepl("site2|site3", message))
  expect_false(grepl("[0-9]+ (levels|rows|persons|concepts)", message))
  expect_true(all(vapply(.cvl_frames(ctx, "Long"), is.null, logical(1))))
})

test_that("calendars without level_range declare every period of their observed years", {
  ctx <- .cvl_setup()
  for (path in ctx$paths) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbExecute(con, "UPDATE death SET death_date = date('2010-01-01', '+' || ((person_id * 37) % 4300) || ' days')")
    DBI::dbDisconnect(con)
  }
  expected <- c(month = 144L, quarter = 48L, iso_week = 626L)
  for (precision in names(expected)) for (mode in c("memory", "staged")) {
    target <- paste0("Dates_", precision, "_", mode)
    recipe <- omop_recipe(variables = list(died = omop_date("death", omop_date_spec(precision = precision))),
      outputs = omop_output(name = "study", type = "wide"))
    recipe_execute(recipe, out = c(study = target), symbol = "cvl", conns = ctx$conns, output_mode = mode)
    levels <- .cvl_shared(ctx, target, "died")
    expect_length(levels, expected[[precision]])
    expect_true(all(substr(levels, 1L, 4L) %in% as.character(2010:2021)), info = target)
  }
})

test_that("longer calendars stay refused, and level_range declares exactly its periods", {
  # Twelve years of weeks are longer than this server declares by itself.
  ctx <- .cvl_setup(auto_max = "100")
  for (path in ctx$paths) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbExecute(con, "UPDATE death SET death_date = date('2010-01-01', '+' || ((person_id * 37) % 4300) || ' days')")
    DBI::dbDisconnect(con)
  }
  recipe <- omop_recipe(variables = list(died = omop_date("death", omop_date_spec(precision = "iso_week"))),
    outputs = omop_output(name = "study", type = "wide"))
  expect_error(recipe_execute(recipe, out = c(study = "Weeks"), symbol = "cvl", conns = ctx$conns),
    "dsomop.auto_declared_levels_max", fixed = TRUE)
  # level_range keeps declaring exactly its own periods.
  recipe <- omop_recipe(variables = list(died = omop_date("death",
      omop_date_spec(precision = "month", level_range = c("2009-01-01", "2022-12-31")))),
    outputs = omop_output(name = "study", type = "wide"))
  recipe_execute(recipe, out = c(study = "Ranged"), symbol = "cvl", conns = ctx$conns)
  expect_length(.cvl_shared(ctx, "Ranged", "died"), 14L * 12L)
})

test_that("year factors over more years than the rule admits declare their decades", {
  ctx <- .cvl_setup()
  for (path in ctx$paths) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbExecute(con, "UPDATE death SET death_date = date('1931-03-01', '+' || ((person_id * 137) % 33000) || ' days')")
    DBI::dbDisconnect(con)
  }
  recipe <- omop_recipe(variables = list(died = omop_date("death", omop_date_spec(precision = "year"))),
    outputs = omop_output(name = "study", type = "wide"))
  recipe_execute(recipe, out = c(study = "Years"), symbol = "cvl", conns = ctx$conns)
  levels <- .cvl_shared(ctx, "Years", "died")
  expect_identical(levels, as.character(1930:2029))
})
