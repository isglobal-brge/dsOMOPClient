# All row inspection is administrator-side test code over public synthetic data.
# Actual analyst calls use normal recipe, DataSHIELD and protected-object routes.
if (nzchar(Sys.getenv("DSOMOP_TEST_NUMERIC_CDM"))) .numeric_definitions()

test_that("numeric recipes preserve baseline/features aliases, A27 categories and dates", {
  ctx <- .numeric_sites()
  lab <- omop_event("measurement", numeric_lab, name = "lab")
  vars <- list(mean_date = omop_variable("mean_date", event = "lab", reducer = omop_reduce("mean")),
    at_index = omop_value_at_index("measurement", numeric_lab, unit = 8713L),
    category = omop_variable("category", table = "person", column = "gender_concept_id"),
    day = omop_date(omop_anchor("lab")), year = omop_date_parts(omop_anchor("lab"), "year"))
  for (format in c("wide", "features", "baseline")) {
    r <- omop_recipe(events = list(lab), populations = omop_population("p", index = omop_index("lab")),
      variables = vars, outputs = omop_output("o", type = format, rows = "p", grain = "episode"))
    previous <- NULL
    for (mode in c("memory", "staged")) {
      recipe_execute(r, out = c(o = "Numeric"), symbol = "numeric", conns = ctx$conns, output_mode = mode)
      frames <- .numeric_frames(ctx, "Numeric")
      for (i in seq_along(frames)) {
        f <- frames[[i]]; d <- numeric_measurements(ctx$pg, ctx$schemas[[i]])
        want <- numeric_oracle(d)
        expect_s3_class(f, "omop.table")
        expect_type(f$mean_date, "double"); expect_type(f$at_index, "double")
        expect_equal(sort(f$mean_date), sort(want$mean))
        expect_equal(sort(f$at_index), sort(want$at_index))
        expect_s3_class(f$day, "Date"); expect_true(is.factor(f$category))
        expect_equal(sort(f$day), unname(sort(as.Date(vapply(split(d$measurement_date, d$person_id), function(x) as.character(min(x)), "")))))
        if (!is.null(previous)) for (col in names(f)) expect_equal(f[[col]], previous[[i]][[col]])
      }
      expect_true(all(vapply(frames, function(f) identical(levels(f$category), levels(frames[[1L]]$category)), logical(1))))
      previous <- frames
    }
  }
})

test_that("numeric plans execute every format at three DSLite sites in both modes", {
  ctx <- .numeric_sites()
  p <- numeric_formats()
  out <- stats::setNames(paste0("Numeric_", unlist(p$publish)), unlist(p$publish))
  previous <- NULL
  for (mode in c("memory", "staged")) {
    ds.omop.plan.execute(p, out = out, symbol = "numeric", conns = ctx$conns, output_mode = mode)
    frames <- lapply(out, function(s) .numeric_frames(ctx, s))
    for (i in seq_along(ctx$servers)) {
      d <- numeric_measurements(ctx$pg, ctx$schemas[[i]]); want <- numeric_oracle(d)
      for (col in setdiff(names(want), "pid")) {
        expect_true(is.numeric(frames$wide[[i]][[col]]))
        expect_equal(sort(frames$wide[[i]][[col]], na.last = TRUE), sort(want[[col]], na.last = TRUE), tolerance = 1e-9)
      }
      expect_type(frames$long[[i]]$value, "double")
      expect_equal(sort(frames$long[[i]]$value), sort(d$value_as_number))
      expect_type(frames$panel[[i]]$lab_mean, "double")
      expect_type(frames$sparse[[i]]$covariate_value, "double")
      expect_type(frames$temporal[[i]]$covariate_value, "double")
      expect_equal(sort(frames$survival_covariates[[i]]$mean), sort(want$mean))
      if (!is.null(previous)) for (root in names(frames)) for (col in names(frames[[root]][[i]])) {
        expect_equal(frames[[root]][[i]][[col]], previous[[root]][[i]][[col]])
      }
    }
    previous <- frames
  }
})

test_that("real dsBase mean quantileMean and glm pool numeric columns at two and three sites", {
  ctx <- .numeric_sites(); .numeric_lexer()
  p <- numeric_plan()
  ds.omop.plan.execute(p, out = c(wide = "Numeric"), symbol = "numeric", conns = ctx$conns)
  frames <- .numeric_frames(ctx, "Numeric")
  for (n in 2:3) {
    conns <- ctx$conns[seq_len(n)]
    pooled <- dsBaseClient::ds.mean("Numeric$mean", type = "combine", datasources = conns)
    quantiles <- dsBaseClient::ds.quantileMean("Numeric$mean", type = "combine", datasources = conns)
    fit <- dsBaseClient::ds.glm(formula = "mean ~ first", data = "Numeric", family = "gaussian", datasources = conns)
    values <- unlist(lapply(frames[seq_len(n)], function(f) f$mean))
    expect_equal(as.numeric(pooled$Global.Mean[1L, 1L]), mean(values), tolerance = 1e-9)
    expect_true(all(is.finite(unlist(quantiles))))
    expected_quantiles <- Reduce(`+`, lapply(frames[seq_len(n)], function(f) {
      stats::quantile(f$mean, c(.05, .10, .25, .50, .75, .90, .95)) * nrow(f)
    })) / length(values)
    expect_equal(unname(quantiles[1:7]), unname(expected_quantiles))
    expect_equal(unname(quantiles[["Mean"]]), mean(values))
    reference <- stats::glm(mean ~ first, data = do.call(rbind, lapply(frames[seq_len(n)], as.data.frame)))
    expect_equal(unname(fit$coefficients[, 1L]), unname(stats::coef(reference)), tolerance = 1e-6)
  }
})
