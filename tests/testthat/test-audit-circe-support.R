.audit_support_definition <- function() {
  list(ConceptSets = list(list(id = 0L, name = "condition",
    expression = list(items = list(list(concept = list(CONCEPT_ID = 201820L)))))),
    PrimaryCriteria = list(CriteriaList = list(list(ConditionOccurrence = list(CodesetId = 0L))),
      PrimaryCriteriaLimit = list(Type = "First"),
      ObservationWindow = list(PriorDays = 0L, PostDays = 0L)))
}

test_that("Circe support inspection distinguishes directions without server calls", {
  support <- recipe_circe_support()
  expect_named(support, c("construct", "import", "legacy_export", "native_export",
    "conditions", "reason_code", "remedy", "adapter_version"))
  expect_true(all(c("Death", "DrugEra", "ConditionEra", "ObservationPeriod", "VisitDetail",
    unlist(.circe_domain_map)) %in% support$construct))
  expect_true(all(support$native_export == "unsupported"))
  for (domain in unlist(.circe_domain_map)) {
    x <- .audit_support_definition()
    names(x$PrimaryCriteria$CriteriaList[[1L]]) <- domain
    check <- recipe_circe_check(x)
    expect_true(check$supported, info = domain)
    expect_equal(nrow(check$issues), 0L)
    expect_s3_class(recipe_import_circe(x), "omop_population")
  }
  expect_error(recipe_circe_check(list(), "IMPORT"), "invalid_direction")
})

test_that("Circe checking reports independent clauses in structural path order", {
  x <- .audit_support_definition()
  x$PrimaryCriteria$CriteriaList <- list(list(Death = list()), list(DrugEra = list()))
  x$PrimaryCriteria$PrimaryCriteriaLimit$Type <- "All"
  x$AdditionalCriteria <- list(Type = "ANY")
  x$CensoringCriteria <- list(list(Death = list()))
  x$CensorWindow <- list(StartDate = "SENTINEL_PRIVATE_TEXT")
  x$InclusionRules <- list(list(name = "SENTINEL_PRIVATE_TEXT", expression = list(
    Type = "ALL", Groups = list(list(Type = "ANY")), CriteriaList = list(
      list(Criteria = list(VisitDetail = list()), EndWindow = list(Start = 1L), RestrictVisit = TRUE),
      list(Criteria = list(ObservationPeriod = list()))))))
  check <- recipe_circe_check(x)
  expect_false(check$supported)
  expect_true(all(c("Death", "DrugEra", "VisitDetail", "ObservationPeriod", "EntryCardinality",
    "FirstLast", "AdditionalCriteria", "CensoringCriteria", "CensorWindow", "Groups",
    "EndWindow", "RestrictVisit") %in% check$issues$construct))
  expect_identical(check$issues$path, sort(check$issues$path, method = "radix"))
  expect_false(any(grepl("SENTINEL", unlist(check), fixed = TRUE)))
  expect_error(recipe_import_circe(x), "recipe_circe_check")
  x$SENTINEL_PRIVATE_FIELD <- TRUE
  expect_false(any(grepl("SENTINEL", unlist(recipe_circe_check(x)), fixed = TRUE)))
})

test_that("malformed and oversized structural input receives a bounded root report", {
  deep <- list()
  for (i in 1:70) deep <- list(child = deep)
  for (x in list('{"broken":', '{"x":1,"x":2}', deep, TRUE,
                 list(PrimaryCriteria = TRUE), list(ConceptSets = 42))) {
    check <- recipe_circe_check(x)
    expect_false(check$supported)
    expect_true(nrow(check$issues) >= 1L)
    expect_lt(nchar(paste(unlist(check), collapse = "")), 2000L)
  }
})

test_that("native export cannot fall back to a stale legacy index", {
  pop <- recipe_import_circe(.audit_support_definition())
  pop$index <- omop_index(omop_event("condition_occurrence", 201826L))
  r <- omop_recipe(populations = pop)
  check <- recipe_circe_check(r, "export")
  expect_false(check$supported)
  expect_true("unsupported_native_export" %in% check$issues$reason_code)
  expect_error(recipe_export_circe(r, "imported"), "unsupported_native_export")
})

test_that("Circe provenance preserves supplied bytes, sanitized origin and derivative identity", {
  x <- .audit_support_definition()
  bytes <- paste0(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)), "\n")
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  writeBin(charToRaw(bytes), path)
  origin <- list(uri = "https://user:password@example.org/phenotype?token=secret#secret",
    phenotype_id = "fixture", release = "pinned", review_status = "unknown")
  pop <- recipe_import_circe(path, origin = origin)
  p <- pop$circe_provenance
  expect_identical(p$original_json, bytes)
  expect_identical(p$original_sha256, .circe_hash(bytes))
  expect_identical(p$origin$uri, "https://example.org/phenotype")
  expect_false(p$derivative)
  expect_identical(recipe_import_circe(x)$circe_provenance$origin_form, "list")
  r <- omop_recipe(populations = pop)
  loaded <- recipe_import_json(recipe_export_json(r))
  expect_identical(loaded$populations$imported$circe_provenance, p)
  r$populations$imported$index_event$concept_id <- 201826L
  edited <- recipe_import_json(recipe_export_json(r))$populations$imported$circe_provenance
  expect_true(edited$derivative)
  expect_identical(edited$original_sha256, p$original_sha256)
  expect_identical(edited$original_recipe_digest, p$original_recipe_digest)
  expect_false(identical(edited$recipe_digest, p$recipe_digest))
  expect_error(recipe_import_circe(x, origin = list(password = "secret")), "invalid_origin")
})

test_that("pinned supported and unsupported PhenotypeLibrary definitions are inspected intact", {
  fixture <- test_path("..", "fixtures", "phenotype-library")
  manifest <- jsonlite::fromJSON(file.path(fixture, "manifest.json"), simplifyVector = FALSE)
  for (pin in manifest$fixtures) {
    path <- file.path(fixture, paste0(pin$id, ".json"))
    bytes <- rawToChar(readBin(path, "raw", n = file.info(path)$size))
    expect_identical(.circe_hash(bytes), pin$sha256)
    check <- recipe_circe_check(path)
    if (pin$id == "304") {
      expect_true(check$supported)
      pop <- recipe_import_circe(path, origin = list(uri = pin$source, phenotype_id = pin$id,
        release = manifest$commit, review_status = pin$status))
      expect_identical(pop$circe_provenance$original_sha256, pin$sha256)
      # The only additional ATLAS field admitted is an explicitly false,
      # empty ConditionType exclusion; true/null/arrays remain unsupported.
      x <- jsonlite::fromJSON(bytes, simplifyVector = FALSE)
      for (v in list(TRUE, NULL, list(FALSE))) {
        x$PrimaryCriteria$CriteriaList[[1L]]$ConditionOccurrence["ConditionTypeExclude"] <- list(v)
        expect_false(recipe_circe_check(x)$supported)
      }
    } else {
      expect_false(check$supported)
      expect_true(all(c("EntryCardinality", "FirstLast", "ExpressionLimit", "UnknownField") %in% check$issues$construct))
      expect_error(recipe_import_circe(path), "recipe_circe_check")
    }
  }
})

test_that("independent bounds occurrence windows and demographics all receive diagnostics", {
  x <- .audit_support_definition()
  x$InclusionRules <- list(list(expression = list(Type = "ALL",
    CriteriaList = list(list(Criteria = list(Measurement = list(CodesetId = 0L,
      ValueAsNumber = list(Op = "bt", Value = 1, Extent = 2))),
      Occurrence = list(Type = 1L, Count = 3L), StartWindow = list(Unsupported = TRUE))),
    DemographicCriteriaList = list(list(Age = list(Op = "unsupported", Value = 5),
      Gender = list(list(CONCEPT_ID = 0L)), Unsupported = "PRIVATE_SENTINEL")))))
  check <- recipe_circe_check(x)
  expect_false(check$supported)
  paths <- check$issues$path
  expect_true(any(endsWith(paths, ".ValueAsNumber")))
  expect_true(any(endsWith(paths, ".Occurrence")))
  expect_true(any(endsWith(paths, ".StartWindow")))
  expect_true(any(endsWith(paths, ".Age")))
  expect_true(any(endsWith(paths, ".Gender")))
  expect_true(any(grepl("field:", paths, fixed = TRUE)))
  expect_false(any(grepl("PRIVATE_SENTINEL", unlist(check), fixed = TRUE)))
  expect_error(recipe_import_circe(x), "recipe_circe_check")
})

test_that("editing imported populations changes the executable declaration and provenance", {
  pop <- recipe_import_circe(.audit_support_definition(), id = "p")
  r <- omop_recipe(populations = pop, variables = omop_variable_sex(),
    outputs = omop_output("o", type = "wide", population_id = "p"))
  original <- recipe_to_plan(r)
  r$populations$p$index_event$concept_id <- 201826L
  edited <- recipe_to_plan(r)
  expect_false(identical(.plan_v2_digest(original)$sha256, .plan_v2_digest(edited)$sha256))
  expect_true(any(vapply(edited$concept_sets, function(cs) 201826L %in% unlist(cs$concepts), logical(1))))
  for (format in c("json", "yaml")) {
    path <- tempfile(fileext = paste0(".", format))
    recipe_save(r, path)
    loaded <- recipe_load(path)
    unlink(path)
    expect_identical(.plan_v2_digest(recipe_to_plan(loaded))$sha256, .plan_v2_digest(edited)$sha256)
    expect_true(loaded$populations$p$circe_provenance$derivative)
    expect_identical(loaded$populations$p$circe_provenance$original_sha256,
      pop$circe_provenance$original_sha256)
  }
  generated <- eval(parse(text = recipe_to_code(r)))
  expect_identical(.plan_v2_digest(recipe_to_plan(generated))$sha256, .plan_v2_digest(edited)$sha256)
  merged <- recipe_merge(r, r, prefix = "copy")
  expect_true(merged$populations$copy_p$circe_provenance$derivative)
  expect_identical(merged$populations$copy_p$circe_provenance$original_sha256,
    pop$circe_provenance$original_sha256)
  merged_plan <- recipe_to_plan(merged)
  expect_true(all(vapply(merged_plan$concept_sets, function(cs) {
    201826L %in% unlist(cs$concepts) && identical(cs$validity_policy, "ohdsi-import-1")
  }, logical(1))))
  rebuilt <- eval(parse(text = recipe_to_code(merged)))
  expect_identical(.plan_v2_digest(recipe_to_plan(rebuilt))$sha256,
    .plan_v2_digest(merged_plan)$sha256)
  # A native override is an explicit choice and must not execute stale Circe.
  r$populations$p$index <- omop_index(omop_event("condition_occurrence", 201821L))
  native <- recipe_to_plan(r)
  expect_true(any(vapply(native$events, function(e) 201821L %in% unlist(e$concept_set$concepts), logical(1))))
})

test_that("conflicting import edits refuse while successive reconciled edits remain usable", {
  pop <- recipe_import_circe(.audit_support_definition(), id = "p")
  pop$index_event$concept_id <- 201826L
  first <- .circe_reconcile_population(pop)
  first$index_event$concept_id <- 201821L
  second <- .circe_reconcile_population(first)
  expect_identical(second$circe_v2$index$source$concepts$concepts, 201821L)
  expect_identical(second$circe_provenance$original_sha256,
    pop$circe_provenance$original_sha256)
  conflict <- pop
  conflict$circe_v2$index$source <- omop_event("condition_occurrence", 201822L)
  err <- tryCatch(.circe_reconcile_population(conflict), error = identity)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "^ambiguous_circe_edit:")
  expect_false(grepl("201826|201822", conditionMessage(err)))
  conflict$circe_v2 <- NULL
  expect_identical(.circe_reconcile_population(conflict), conflict)
})

test_that("explicit coverage overrides imported defaults through every recipe representation", {
  pop <- recipe_import_circe(.audit_support_definition(), id = "p")
  r <- omop_recipe(populations = pop,
    outputs = omop_output("o", rows = "p", format = "wide"))
  original <- recipe_to_plan(r, version = 2)
  expect_identical(original$populations$p$coverage$policy, "unique")
  r$populations$p$coverage <- omop_coverage("earliest")
  edited <- recipe_to_plan(r, version = 2)
  digest <- function(r) .plan_v2_digest(recipe_to_plan(r, version = 2))$sha256
  expect_identical(edited$populations$p$coverage$policy, "earliest")
  expect_false(identical(.plan_v2_digest(original)$sha256, .plan_v2_digest(edited)$sha256))
  for (format in c("json", "yaml")) {
    path <- tempfile(fileext = paste0(".", format))
    recipe_save(r, path)
    loaded <- recipe_load(path)
    unlink(path)
    expect_identical(digest(loaded), digest(r))
    prov <- loaded$populations$p$circe_provenance
    expect_true(prov$derivative)
    expect_identical(prov$original_json, pop$circe_provenance$original_json)
    expect_identical(prov$original_sha256, pop$circe_provenance$original_sha256)
    expect_identical(prov$original_recipe_digest, pop$circe_provenance$original_recipe_digest)
    expect_identical(prov$recipe_digest, .circe_population_digest(loaded$populations$p))
  }
  rebuilt <- eval(parse(text = recipe_to_code(r)))
  expect_identical(digest(rebuilt), digest(r))
  expect_true(rebuilt$populations$p$circe_provenance$derivative)
  # Orthogonal legacy edits cannot reset the explicit native policy.
  r$populations$p$index_event$concept_id <- 201826L
  expect_identical(recipe_to_plan(r)$populations$p$coverage$policy, "earliest")
  r$populations$p$coverage <- omop_coverage("union", gap = 7L)
  expect_identical(recipe_to_plan(r)$populations$p$coverage$policy, "union")
  expect_equal(recipe_to_plan(r)$populations$p$coverage$gap, 7L)
  # Even a directly changed translation is subordinate to the explicit policy.
  r$populations$p$index_event <- pop$index_event
  r$populations$p$circe_v2$coverage <- omop_coverage("earliest")
  r$populations$p$coverage <- omop_coverage("unique")
  expect_identical(recipe_to_plan(r)$populations$p$coverage$policy, "unique")
  r$populations$p$coverage <- NULL
  expect_identical(recipe_to_plan(r)$populations$p$coverage$policy, "earliest")
  # Malformed overrides are validated, never quietly replaced by the cache.
  r$populations$p$coverage <- list(policy = "PRIVATE_SENTINEL")
  expect_error(recipe_to_plan(r))
})

test_that("nested groups report independent clauses at every depth without echoing input", {
  x <- .audit_support_definition()
  leaf <- list(Type = "ALL", CriteriaList = list(list(Criteria = list(Measurement = list(
    CodesetId = 0L, ValueAsNumber = list(Op = "bt", Value = 1, Extent = 2))),
    Occurrence = list(Type = 1L, Count = 3L), StartWindow = list(Unsupported = TRUE))),
    DemographicCriteriaList = list(list(Age = list(Op = "unsupported", Value = 5),
      Gender = list(list(CONCEPT_ID = 0L)), PRIVATE_SENTINEL = TRUE)))
  x$InclusionRules <- list(list(expression = list(Type = "ALL", Groups = list(
    leaf, list(Type = "ANY", Groups = list(leaf))))))
  check <- recipe_circe_check(x)
  expect_false(check$supported)
  for (prefix in c("$.InclusionRules[1].expression.Groups[1]",
                   "$.InclusionRules[1].expression.Groups[2].Groups[1]")) {
    paths <- check$issues$path[startsWith(check$issues$path, prefix)]
    for (suffix in c(".ValueAsNumber", ".Occurrence", ".StartWindow", ".Age", ".Gender")) {
      expect_true(any(endsWith(paths, suffix)), info = paste(prefix, suffix))
    }
  }
  expect_identical(check$issues$path, sort(check$issues$path, method = "radix"))
  expect_false(any(grepl("PRIVATE_SENTINEL", unlist(check), fixed = TRUE)))
  expect_error(recipe_import_circe(x), "recipe_circe_check")
})

test_that("Circe export refuses unrepresentable native coverage while native edits compile", {
  r <- omop_recipe(populations = recipe_import_circe(.audit_support_definition(), id = "p"),
    outputs = omop_output("o", rows = "p", format = "wide"))
  r$populations <- r$populations["p"]
  r$populations$p$coverage <- omop_coverage("earliest")
  check <- recipe_circe_check(r, "export")
  expect_false(check$supported)
  expect_true(any(check$issues$construct == "Coverage"))
  expect_error(recipe_export_circe(r, population_id = "p"), "observation policy.*recipe_save")
  r$populations$p$index_event$concept_id <- 201826L
  expect_identical(recipe_to_plan(r)$populations$p$coverage$policy, "earliest")
  r$populations$p$coverage <- omop_coverage("unique")
  expect_true(recipe_circe_check(r, "export")$supported)
})

test_that("malformed nested clauses do not erase diagnostics for their siblings", {
  x <- .audit_support_definition()
  x$InclusionRules <- list(list(expression = list(Type = "ALL", Groups = list(
    list(Type = "ALL", CriteriaList = list(list(Criteria = list(Measurement = "PRIVATE_SENTINEL"))),
      DemographicCriteriaList = list(list(Age = list(Op = "unsupported", Value = 5)))),
    list(Type = "ANY", CriteriaList = list(list(Criteria = list(Death = list()))))))))
  issues <- recipe_circe_check(x)$issues
  expect_true(any(endsWith(issues$path, ".Measurement")))
  expect_true(any(endsWith(issues$path, ".Age")))
  expect_true(any(endsWith(issues$path, ".Death")))
  expect_false(any(issues$path == "$"))
  expect_false(any(grepl("PRIVATE_SENTINEL", unlist(issues), fixed = TRUE)))
})
