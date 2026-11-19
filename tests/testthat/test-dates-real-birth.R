# dsOMOP 2.10.7: whole-number REAL/NUMERIC birth components, common in ETL
# exports, give ages and dates. A value that is not a whole number is an
# invalid component (NA), like any other, never a refusal. Fixture:
# helper-concept-levels.R.

.real_birth <- function(ctx) {
  for (path in ctx$paths) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    ddl <- DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'person'")$sql[[1L]]
    ddl <- gsub("(year_of_birth|month_of_birth|day_of_birth) INTEGER", "\\1 REAL", ddl)
    DBI::dbExecute(con, "ALTER TABLE person RENAME TO real_old")
    DBI::dbExecute(con, ddl)
    DBI::dbExecute(con, "INSERT INTO person SELECT * FROM real_old")
    DBI::dbExecute(con, "DROP TABLE real_old")
    DBI::dbExecute(con, "UPDATE person SET year_of_birth = 1980.0, month_of_birth = 6.0, day_of_birth = 15.0")
    DBI::dbExecute(con, "UPDATE person SET year_of_birth = 1980.5 WHERE person_id = 1")
    types <- DBI::dbGetQuery(con, "PRAGMA table_info(person)")
    expect_identical(types$type[types$name == "year_of_birth"], "REAL")
    DBI::dbDisconnect(con)
  }
}

test_that("REAL birth components give ages; a value that is not whole is NA", {
  ctx <- .cvl_setup()
  .real_birth(ctx)
  recipe <- omop_recipe(variables = list(age = omop_age_at("death"),
      born = omop_date("birth", omop_date_spec(precision = "year"))),
    outputs = omop_output(name = "study", type = "wide"))
  recipe_execute(recipe, out = c(study = "Ages"), symbol = "cvl", conns = ctx$conns)
  for (frame in .cvl_frames(ctx, "Ages")) {
    expect_true(is.numeric(frame$age))
    # Born 1980-06-15, died 2021-01-01.
    expect_true(all(frame$age[!is.na(frame$age)] == 40))
    expect_identical(sum(is.na(frame$age)), 1L)
    expect_identical(unique(stats::na.omit(as.character(frame$born))), "1980")
    expect_identical(sum(is.na(frame$born)), 1L)
  }
})
