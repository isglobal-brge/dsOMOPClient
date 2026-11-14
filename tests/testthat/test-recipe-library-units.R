test_that("numeric library scale rules survive every binding route", {
  for (id in c("panel.measurements", "socioeconomic.numeric_observation")) {
    bad <- omop_event("measurement", 990003, name = "measure")
    index <- omop_event("visit_occurrence", 9201)
    expect_error(recipe_library_instantiate(id, index = index, measure = bad), "scale/unit policy")
    base <- recipe_library_instantiate(id, index = index)
    for (r in list(base, recipe_import_json(recipe_export_json(base)),
                   recipe_import_yaml(recipe_export_yaml(base)),
                   recipe_merge(omop_recipe(), base, prefix = "lib"))) {
      expect_error(recipe_bind(r, measure = bad), "scale/unit policy")
    }
    policies <- list(
      list(unit = omop_unit(8840)),
      list(unit = omop_unit(8840, convert = list("8554" = c(factor = .001, offset = 0)), unknown = "exclude")),
      list(where = omop_where(unit_concept_id == 8840)),
      list(where = omop_where(unit_concept_id %in% c(8840))),
      list(where = omop_where(unit_concept_id == 8840 & value > 0)),
      list(where = omop_where((unit_concept_id == 8840 & value > 0) | (unit_concept_id == 8840 & value < 0))))
    for (policy in policies) {
      event <- do.call(omop_event, c(list("measurement", 990003, name = "measure"), policy))
      r <- recipe_bind(base, measure = event)
      expect_s3_class(recipe_to_plan(r), "omop_plan_v2")
      deferred <- recipe_bind(base, measure = "measure")
      deferred <- recipe_add_event(deferred, event)
      expect_identical(.library_test_digest(recipe_to_plan(deferred)), .library_test_digest(recipe_to_plan(r)))
      for (rt in list(deferred, recipe_import_json(recipe_export_json(deferred)),
                      recipe_import_yaml(recipe_export_yaml(deferred)))) {
        before <- recipe_merge(omop_recipe(), rt, prefix = "lib")
        after <- recipe_bind(recipe_merge(omop_recipe(), recipe_add_event(base, event), prefix = "lib"), measure = "measure")
        expect_identical(.library_test_digest(recipe_to_plan(before)), .library_test_digest(recipe_to_plan(after)))
      }
    }
    for (where in list(NULL, omop_where(unit_concept_id %in% c(8840, 8554)),
      omop_where(unit_concept_id == 8840 | value > 0), omop_where(unit_concept_id != 8554))) {
      event <- omop_event("measurement", 990003, name = "measure", where = where)
      r <- recipe_add_event(recipe_bind(base, measure = "measure"), event)
      for (rt in list(r, recipe_import_json(recipe_export_json(r)),
                      recipe_merge(omop_recipe(), r, prefix = "lib"))) {
        expect_error(recipe_to_plan(rt), "scale/unit policy")
      }
    }
  }
})

test_that("mixed-unit library execution refuses unqualified values and honours explicit policy", {
  ctx <- .library_cdm_setup()
  session <- dsOMOPClient:::.get_session(ctx$symbol)
  server <- ctx$servers[[1L]]
  handle <- server$getSessionData(server$getSessionIds()[[1L]], paste0(".dsomop_handle_", session$res_symbol))
  DBI::dbExecute(handle$conn, "UPDATE measurement SET unit_concept_id=8554,value_as_number=8000 WHERE measurement_date='2020-01-11'")
  r <- recipe_library_instantiate("panel.measurements", index = omop_event("visit_occurrence", 9201), measure = "measure")
  bad <- recipe_add_event(r, omop_event("measurement", 990003, name = "measure"))
  before <- DSI::datashield.symbols(ctx$conns)
  expect_error(recipe_execute(bad, out = c(measurements = "D_unit_bad"), symbol = ctx$symbol, conns = ctx$conns), "scale/unit policy")
  expect_identical(DSI::datashield.symbols(ctx$conns), before)
  policies <- list(convert = list(unit = omop_unit(8840, convert = list("8554" = c(factor = .001, offset = 0)))),
    restrict = list(where = omop_where(unit_concept_id == 8840)))
  for (nm in names(policies)) {
    good <- recipe_add_event(r, do.call(omop_event, c(list("measurement", 990003, name = "measure"), policies[[nm]])))
    sym <- paste0("D_unit_", nm)
    recipe_execute(good, out = c(measurements = sym), symbol = ctx$symbol, conns = ctx$conns)
    f <- .library_frames(ctx, sym)[[1L]]
    f <- f[f$period_start_days == 0, ]
    expect_equal(f$measure_count, rep(if (nm == "convert") 2L else 1L, 24))
    expect_equal(f$measure_mean, rep(if (nm == "convert") 7 else 6, 24))
    expect_equal(f$measure_last, rep(if (nm == "convert") 8 else 6, 24))
    expect_equal(f$measure_carry, rep(6, 24))
  }
})

test_that("numeric social observations declare their unitless scale explicitly", {
  r <- recipe_library_instantiate("socioeconomic.numeric_observation",
    index = omop_event("visit_occurrence", 9201))
  expect_error(recipe_bind(r, measure = omop_event("observation", 990001)), "scale/unit policy")
  for (measure in list(
    omop_event("observation", 990001, unit = omop_unit(0)),
    omop_event("observation", 990001, where = omop_where(unit_concept_id == 0)))) {
    expect_s3_class(recipe_to_plan(recipe_bind(r, measure = measure)), "omop_plan_v2")
  }
})
