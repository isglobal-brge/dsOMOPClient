test_that("all 24 installed definitions are discoverable ordinary recipes", {
  catalog <- recipe_library_list()
  expect_equal(nrow(catalog), 24L)
  expect_false(anyDuplicated(catalog$id) > 0L)
  for (id in catalog$id) {
    r <- recipe_library_instantiate(id)
    expect_s3_class(r, "omop_recipe")
    expect_true(length(recipe_params(r)) > 0L, info = id)
    d <- recipe_library_describe(id)
    expect_match(d$definition_checksum, "^[a-f0-9]{64}$")
    expect_true(nzchar(d$documentation), info = id)
    expect_true(length(d$output_schema) > 0L, info = id)
    expect_error(recipe_to_plan(r, version = 2L), "Unbound parameter")
  }
  expect_equal(nrow(recipe_library_list(category = "demographics")), 2L)
  expect_equal(nrow(recipe_library_list(category = c("demographics", "drug"))), 4L)
  expect_equal(recipe_library_list(query = "CHARLSON")$id, "comorbidity.charlson")
  expect_equal(nrow(recipe_library_list(query = ".*")), 0L)
  expect_equal(nrow(recipe_library_list(category = "drug", query = "education")), 0L)
  expect_identical(recipe_library_list(versions = "all"), catalog)
  expect_error(recipe_library_instantiate("baseline"), "Unknown installed")
  expect_error(recipe_library_describe("demographics.baseline", "2.0.0"), "Unknown installed")
})

for (.library_id in c("demographics.baseline", "demographics.death_cause",
  "socioeconomic.education", "socioeconomic.employment", "socioeconomic.numeric_observation",
  paste0("comorbidity.", c("charlson", "chads2", "chadsvasc", "dcsi", "hfrs", "condition_history")),
  "drug.era_summary", "drug.adherence_components", "cohort.new_users", "cohort.active_comparator",
  "outcome.incident_event", "survival.first_event", "survival.competing_risk", "survival.recurrent_events",
  "panel.measurements", "panel.incidence", "panel.sccs", "longitudinal.event_log", "matching.nested_case_control")) {
  local({
    id <- .library_id
    test_that(paste(id, "has a frozen plan, normalized payload and semantic digest"), {
      dir <- .library_test_dir(id)
      r <- .library_test_recipe(id)
      p <- recipe_to_plan(r, version = 2L)
      frozen <- .library_test_json(file.path(dir, "plan.json"))
      expect_identical(.library_test_canonical(dsOMOPClient:::.v2_plain(p)), .library_test_canonical(frozen))
      normal <- dsOMOPClient:::.planNormalizeV2(p)
      expect_identical(.library_test_canonical(normal), .library_test_canonical(.library_test_json(file.path(dir, "normalized.json"))))
      expect_identical(.library_test_canonical(normal), .library_test_canonical(dsOMOPClient:::.planNormalizeV2(normal)))
      digest <- .library_test_digest(p)
      expect_identical(digest$sha256, readLines(file.path(dir, "semantic.sha256")))
      expect_identical(digest$payload, .library_test_canonical(.library_test_json(file.path(dir, "semantic.json"))))
      expect_true(dsOMOPClient:::.jsonSchemaValidate(dsOMOPClient:::.v2_plan_schema(), dsOMOPClient:::.v2_plain(p))$valid)
      if (requireNamespace("dsOMOP", quietly = TRUE)) {
        expect_identical(.library_test_canonical(normal), .library_test_canonical(dsOMOP:::.planNormalizeV2(p)))
        expect_identical(digest$sha256, dsOMOP:::.planNormalizedDigest(p)$sha256)
      }
      # Literal constructor control is a checked-in ordinary recipe, without
      # catalogue resolution or binding at test time.
      control <- eval(parse(file = file.path(dir, "control.R")))
      variants <- list(control, .library_test_recipe(id, TRUE),
        recipe_import_json(recipe_export_json(r)), recipe_import_yaml(recipe_export_yaml(r)),
        eval(parse(text = recipe_to_code(r))))
      staged <- recipe_library_instantiate(id)
      for (nm in names(.library_test_bindings(id, TRUE))) {
        staged <- do.call(recipe_bind, c(list(staged), .library_test_bindings(id, TRUE)[nm]))
      }
      variants <- c(variants, list(staged))
      for (variant in variants) expect_identical(.library_test_digest(recipe_to_plan(variant, version = 2L)), digest)
      rt <- recipe_import_json(recipe_export_json(r))
      expect_identical(.library_test_canonical(rt$meta$recipe_library), .library_test_canonical(r$meta$recipe_library))
      rt$meta$recipe_library[[1]]$documentation_revision <- 999L
      expect_identical(.library_test_digest(recipe_to_plan(rt)), digest)
      # Every entry has a used parameter whose change changes semantics.
      b <- .library_test_bindings(id, TRUE)
      nm <- if ("lookback" %in% names(b)) "lookback" else if ("horizon" %in% names(b)) "horizon" else
        if ("seed" %in% names(b)) "seed" else if ("gap" %in% names(b)) "gap" else
        if ("washout_days" %in% names(b)) "washout_days" else if ("select" %in% names(b)) "select" else "source_b"
      b[[nm]] <- if (nm == "select") "all" else if (nm == "source_b") omop_event("condition_occurrence", 990008, name = "history") else b[[nm]] + 1L
      changed <- do.call(recipe_library_instantiate, c(list(id), b))
      expect_false(identical(.library_test_digest(recipe_to_plan(changed))$sha256, digest$sha256))
    })
  })
}

test_that("binding enforces catalogue constraints initially, later and after import", {
  for (r in list(recipe_library_instantiate("demographics.baseline"),
    recipe_import_json(recipe_export_json(recipe_library_instantiate("demographics.baseline"))),
    recipe_import_yaml(recipe_export_yaml(recipe_library_instantiate("demographics.baseline"))))) {
    expect_error(recipe_bind(r, lookback = 0L), "at least 1")
    expect_error(recipe_bind(r, select = "nth"), "must be one of")
    expect_error(recipe_bind(r, lookback = 1.5), "integer")
    expect_error(recipe_bind(r, unknown = 1), "not a parameter")
    expect_error(recipe_bind(r, 2), "by name")
    expect_error(recipe_bind(r, lookback = 1, lookback = 2), "once")
  }
  expect_error(recipe_library_instantiate("panel.measurements", measure = omop_event("condition_occurrence", 1)), "numeric")
  expect_error(recipe_bind(recipe_library_instantiate("panel.measurements"),
    measure = omop_event("measurement", 1, name = "other")), "source name")
  expect_error(recipe_library_instantiate("panel.measurements", measure = omop_event("measurement", name = "measure")), "finite concept")
  expect_error(recipe_library_instantiate("socioeconomic.numeric_observation", measure = omop_event("observation", 1, value = "value_as_concept_id")), "numeric")
  expect_error(recipe_library_instantiate("cohort.active_comparator", washout_days = 0), "at least 1")
  expect_error(recipe_library_instantiate("drug.adherence_components", horizon = 0), "at least 1")
})

test_that("all definitions retain deferred prefix semantics and origin provenance", {
  for (id in recipe_library_list()$id) {
    r <- recipe_library_instantiate(id)
    bound <- .library_test_recipe(id)
    before <- recipe_merge(omop_recipe(), bound, prefix = "lib")
    after <- do.call(recipe_bind, c(list(recipe_merge(omop_recipe(), r, prefix = "lib")), .library_test_bindings(id)))
    expect_identical(.library_test_digest(recipe_to_plan(before)), .library_test_digest(recipe_to_plan(after)), info = id)
    expect_equal(before$meta$recipe_library[[1L]]$id, id)
  }
  merged <- recipe_merge(omop_recipe(), recipe_library_instantiate("demographics.baseline"), prefix = "lib")
  expect_error(recipe_bind(recipe_import_json(recipe_export_json(merged)), lookback = 0), "at least 1")
  expect_error(recipe_merge(recipe_library_instantiate("demographics.baseline"),
    recipe_library_instantiate("panel.measurements"), prefix = "m"), "different definitions")
})

test_that("deferred numeric event references receive the same contextual checks", {
  r <- recipe_library_instantiate("socioeconomic.numeric_observation", measure = "social")
  r <- recipe_add_event(r, omop_event("observation", 1, value = "value_as_concept_id", name = "social"))
  r <- recipe_bind(r, index = omop_event("visit_occurrence", 9201))
  expect_error(recipe_to_plan(r), "numeric observation")
})

test_that("artifact definitions and catalogue metadata cannot silently diverge", {
  entries <- dsOMOPClient:::.library_catalog()
  expect_error(dsOMOPClient:::.library_validate_catalog(c(entries,entries[1])),"Duplicate")
  malformed <- entries; malformed[[1]]$template_version <- "latest"
  expect_error(dsOMOPClient:::.library_validate_catalog(malformed),"Malformed")
  for (entry in entries) {
    d <- recipe_library_describe(entry$id)
    file <- system.file("recipe-library",entry$id,entry$template_version,"definition.R",package="dsOMOPClient")
    control <- eval(parse(file=file))
    expect_identical(dsOMOPClient:::.library_checksum(control,d$constraints),d$definition_checksum)
    expect_true(length(entry$required_tables$fixed) > 0L)
  }
})

test_that("edits stay edits through library serialization and new-user zero is meaningful", {
  r <- .library_test_recipe("demographics.baseline")
  r <- recipe_remove_variable(r,"race")
  r <- recipe_remove_variable(r,"ethnicity")
  imported <- recipe_import_json(recipe_export_json(r))
  expect_false("race" %in% names(imported$variables))
  expect_false("ethnicity" %in% names(imported$variables))
  code <- eval(parse(text=recipe_to_code(r)))
  expect_false("race" %in% names(code$variables))
  nu <- recipe_library_instantiate("cohort.new_users",drug=990006,washout_days=0L)
  p <- recipe_to_plan(nu)
  expect_null(p$populations$new_users$where)
  expect_identical(p$populations$new_users$index$end$strategy,"interval_end")
  expect_identical(p$intervals[[1L]]$by,"ingredient")
})
