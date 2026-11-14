test_that("published categorical values retain pooled domains over DSLite and staging", {
  skip_if_not_installed("dsBaseClient"); skip_if_not_installed("dsBase")
  withr::local_options(list(datashield.env = globalenv()))
  lexer <- get("Lexer", asNamespace("DSLite"))
  old_double <- lexer$public_fields$t_DBLSTRING; old_single <- lexer$public_fields$t_STRING
  withr::defer({ lexer$public_fields$t_DBLSTRING <- old_double; lexer$public_fields$t_STRING <- old_single })
  lexer$public_fields$t_DBLSTRING <- r"("([^"\\]|\\.)*")"
  lexer$public_fields$t_STRING <- r"('([^'\\]|\\.)*')"
  ctx <- .clf_setup()
  db <- DBI::dbConnect(RSQLite::SQLite(), ctx$paths[[1L]])
  DBI::dbExecute(db, "UPDATE person SET gender_concept_id = 8532 WHERE person_id <= 9")
  DBI::dbDisconnect(db)
  for (i in seq_along(ctx$paths)) {
    db <- DBI::dbConnect(RSQLite::SQLite(), ctx$paths[[i]])
    DBI::dbExecute(db, paste("UPDATE person SET year_of_birth =", if (i == 2L) 1985L else 1980L))
    DBI::dbDisconnect(db)
  }
  p <- recipe_to_plan(.clf_recipe())
  p$variables$sex <- list(derived = list(kind = "sex_mf"))
  p$variables$age <- list(derived = list(kind = "age", reference_date = "2020-01-01"))
  p$variables$birth_band <- list(derived = list(kind = "birth_year_band", width = 5L))
  p$variables$band <- list(derived = list(kind = "age", reference_date = "2020-01-01", bands = TRUE, age_breaks = list(0L, 18L, 65L)))
  for (part in c("month", "weekday", "season")) p$variables[[part]] <- list(derived = list(kind = "date_part", reference = list(date = "2020-01-01"), part = part))
  p$variables$period <- list(derived = list(kind = "calendar_date", reference = list(date = "2020-01-01")), date_spec = list(precision = "quarter", level_range = list("2020-01-01", "2020-12-31")))
  dsOMOP:::.irValidateWire(dsOMOP:::.irJsonForm(p))
  for (mode in c("memory", "staged")) {
    ds.omop.plan.execute(p, out = c(output = "Categories"), symbol = "labels", conns = ctx$conns, output_mode = mode)
    frames <- .clf_frames(ctx, "Categories")
    for (col in c("education", "cause", "sex", "band", "birth_band", "month", "weekday", "season", "period")) {
      for (frame in frames) {
        expect_true(is.factor(frame[[col]]), info = paste(mode,col))
        expect_identical(is.ordered(frame[[col]]), !col %in% c("education","cause","sex"), info = paste(mode,col))
        expect_identical(levels(frame[[col]]), levels(frames[[1L]][[col]]), info = paste(mode,col))
      }
    }
    expect_identical(levels(frames[[2L]]$sex), c("F", "M"))
    expect_equal(sum(frames[[2L]]$sex == "F"), 0L)
    expect_identical(levels(frames[[1L]]$birth_band), c("1980", "1985"))
    expect_equal(sum(frames[[1L]]$birth_band == "1985"), 0L)
    expect_true("declared only" %in% levels(frames[[1L]]$education))
    if (mode == "memory") {
      lev <- dsBaseClient::ds.levels("Categories$sex", datasources = ctx$conns)
      expect_true(all(vapply(lev, function(x) identical(x$Levels, c("F", "M")), TRUE)))
      tab <- dsBaseClient::ds.table("Categories$sex", datasources = ctx$conns)
      expect_match(tab$validity.message, "all studies were valid")
      fit <- dsBaseClient::ds.glm("age ~ sex", data = "Categories", family = "gaussian", datasources = ctx$conns)
      reference <- stats::glm(age ~ sex, data = do.call(rbind, frames), family = stats::gaussian())
      expect_equal(unname(fit$coefficients[,1L]), unname(stats::coef(reference)), tolerance = 1e-6)
    }
  }
  # Turning off concept recoding must not disable non-concept categories.
  p$variables <- p$variables[c("sex", "band", "birth_band")]
  p$options <- list(translate_concepts = FALSE, factor_concepts = FALSE)
  for (mode in c("memory", "staged")) {
    ds.omop.plan.execute(p, out = c(output = "RawCategories"), symbol = "labels", conns = ctx$conns, output_mode = mode)
    frames <- .clf_frames(ctx, "RawCategories")
    for (frame in frames) {
      expect_true(is.factor(frame$sex)); expect_true(is.ordered(frame$band)); expect_true(is.ordered(frame$birth_band))
      expect_identical(levels(frame$birth_band), c("1980", "1985"))
    }
  }

})
