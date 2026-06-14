# ==============================================================================
# Multi-population + set-op + recipe-level scope: client authoring + transport
#
# The client's job is to AUTHOR the multi-population contract and serialize it
# faithfully; the server EXECUTES it (see dsOMOP/tests/testthat/test-recipe-
# setops.R for the execution proof). These tests therefore assert, with no live
# DataSHIELD backend:
#
#   - the maintainer's headline query, built via the DECLARATIVE
#     omop_recipe(populations = list(...)), COMPILES (recipe_to_plan) into the
#     exact plan the server consumes (set-op kinds, members, criteria
#     filter_trees, per-output population_id, scope) — no "not yet executable"
#     stop;
#   - omop_population() set-op authoring (union / intersect / setdiff) validates
#     fail-closed (one op, >= 2 members, never mixed with filters);
#   - recipe-level scope serializes to plan$scope, and ds.omop.plan.execute()
#     SPLICES the cohort literal + omop.table SYMBOLS into the omopPlanExecuteDS
#     call by name (they cannot ride in the plan JSON) with combine passed by
#     name — the ds.omop.analysis.run scope contract;
#   - JSON/YAML and code round-trip set-op populations and scope;
#   - single-population (base-only) recipes are byte-for-byte unchanged.
#
# recipe_to_plan() is connection-free; the transport test mocks DSI so the
# generated assign expression can be captured and asserted.
# ==============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

strip_meta <- function(r) { r$meta <- NULL; r }

# Build the maintainer's headline recipe declaratively. Reused by several tests.
#   A = women + conditionX(201820) + HbA1c(3004410) high
#   B = women + conditionY(255573) + HbA1c low
#   union_AB = union(A, B)
#   final = intersect(union_AB, hasMeasZ = women + creatinine 3025315)
#   output: wide table over `final`.
headline_recipe <- function() {
  omop_recipe(
    populations = list(
      omop_population("A", "women conditionX + HbA1c high",
        filters = list(
          omop_filter_sex("F"),
          omop_filter_has_concept(201820, "condition_occurrence"),
          omop_filter_has_measurement(3004410, min_value = 7.0))),
      omop_population("B", "women conditionY + HbA1c low",
        filters = list(
          omop_filter_sex("F"),
          omop_filter_has_concept(255573, "condition_occurrence"),
          omop_filter_has_measurement(3004410, max_value = 5.0))),
      omop_population("union_AB", "A or B", union = c("A", "B")),
      omop_population("hasMeasZ", "women creatinine present",
        filters = list(
          omop_filter_sex("F"),
          omop_filter_has_measurement(3025315))),
      omop_population("final", "union_AB and hasMeasZ",
        intersect = c("union_AB", "hasMeasZ"))),
    variables = omop_variable_age(),
    outputs = omop_output(name = "study", type = "wide",
                          population_id = "final"))
}

# ---------------------------------------------------------------------------
# (headline) the declarative headline query COMPILES into the server contract.
# ---------------------------------------------------------------------------

test_that("maintainer headline query builds and compiles to the server contract", {
  recipe <- headline_recipe()
  expect_s3_class(recipe, "omop_recipe")
  expect_equal(length(recipe$populations), 6L)   # base + 5 declared

  # The two set-op populations carry their algebra; the criteria ones do not.
  expect_identical(recipe$populations$union_AB$setop,
                   list(op = "union", members = c("A", "B")))
  expect_identical(recipe$populations$final$setop,
                   list(op = "intersect", members = c("union_AB", "hasMeasZ")))
  expect_null(recipe$populations$A$setop)

  # COMPILES (no "Multiple populations are not yet executable" stop).
  plan <- recipe_to_plan(recipe)
  expect_s3_class(plan, "omop_plan")

  # Every population serialized, with the right kind.
  expect_setequal(names(plan$populations),
                  c("base", "A", "B", "union_AB", "hasMeasZ", "final"))
  expect_identical(plan$populations$A$kind, "criteria")
  expect_false(is.null(plan$populations$A$filter_tree))
  expect_identical(plan$populations$union_AB$kind, "setop")
  expect_identical(plan$populations$union_AB$setop$op, "union")
  expect_identical(plan$populations$union_AB$setop$members, c("A", "B"))
  expect_identical(plan$populations$final$kind, "setop")
  expect_identical(plan$populations$final$setop$op, "intersect")
  expect_identical(plan$populations$final$setop$members,
                   c("union_AB", "hasMeasZ"))

  # The criteria filter_tree the server consumes is the same shape its base
  # cohort uses: an AND of sex + has_concept + has_measurement leaves.
  ft <- plan$populations$A$filter_tree
  expect_true("and" %in% names(ft))
  leaf_types <- vapply(ft$and, function(n) n$type %||% NA_character_, character(1))
  expect_setequal(leaf_types, c("sex", "has_concept", "has_measurement"))

  # The single wide output is stamped with the population it targets.
  expect_identical(plan$outputs[[names(plan$outputs)[1]]]$population_id, "final")
})

test_that("declarative populations=list(...) matches the step-by-step build", {
  decl <- headline_recipe()

  step <- omop_recipe(variables = omop_variable_age())
  step <- dsOMOPClient:::recipe_add_population(step, omop_population(
    "A", "women conditionX + HbA1c high",
    filters = list(omop_filter_sex("F"),
                   omop_filter_has_concept(201820, "condition_occurrence"),
                   omop_filter_has_measurement(3004410, min_value = 7.0))))
  step <- dsOMOPClient:::recipe_add_population(step, omop_population(
    "B", "women conditionY + HbA1c low",
    filters = list(omop_filter_sex("F"),
                   omop_filter_has_concept(255573, "condition_occurrence"),
                   omop_filter_has_measurement(3004410, max_value = 5.0))))
  step <- dsOMOPClient:::recipe_add_population(step, omop_population(
    "union_AB", "A or B", union = c("A", "B")))
  step <- dsOMOPClient:::recipe_add_population(step, omop_population(
    "hasMeasZ", "women creatinine present",
    filters = list(omop_filter_sex("F"),
                   omop_filter_has_measurement(3025315))))
  step <- dsOMOPClient:::recipe_add_population(step, omop_population(
    "final", "union_AB and hasMeasZ", intersect = c("union_AB", "hasMeasZ")))
  step <- dsOMOPClient:::recipe_add_output(step, omop_output(
    name = "study", type = "wide", population_id = "final"))

  expect_identical(strip_meta(decl), strip_meta(step))
  expect_identical(recipe_to_plan(decl), recipe_to_plan(step))
})

# ---------------------------------------------------------------------------
# (a/b) set-op authoring + validation (fail-closed).
# ---------------------------------------------------------------------------

test_that("omop_population set-op accepts union / intersect / setdiff", {
  for (op in c("union", "intersect", "setdiff")) {
    args <- stats::setNames(list(c("x", "y")), op)
    pop <- do.call(omop_population, c(list(id = "r", label = "R"), args))
    expect_identical(pop$setop, list(op = op, members = c("x", "y")))
    expect_length(pop$filters, 0L)
  }
})

test_that("omop_population set-op validation is fail-closed", {
  # More than one set op.
  expect_error(omop_population("r", union = c("a", "b"),
                               intersect = c("a", "b")),
               "only one of union / intersect / setdiff")
  # Fewer than two members.
  expect_error(omop_population("r", union = "a"),
               "at least two member")
  # Mixed with filters.
  expect_error(omop_population("r", union = c("a", "b"),
                               filters = list(omop_filter_sex("F"))),
               "cannot also take filters")
  # Mixed with a cohort_definition_id.
  expect_error(omop_population("r", union = c("a", "b"),
                               cohort_definition_id = 1),
               "cannot also take filters or cohort_definition_id")
})

test_that("recipe_add_population rejects a set-op over undeclared members", {
  expect_error(
    omop_recipe(populations = list(
      omop_population("only", "Only", filters = list(omop_filter_sex("F"))),
      omop_population("r", "R", union = c("only", "ghost")))),
    "unknown member")
})

test_that("recipe_to_plan fails closed on an output targeting an undeclared pop", {
  expect_error(
    recipe_to_plan(omop_recipe(
      variables = omop_variable_age(),
      outputs = omop_output(name = "w", type = "wide",
                            population_id = "ghost"))),
    "undeclared population")
})

# ---------------------------------------------------------------------------
# (c) recipe-level scope serialization + the execute-path splice transport.
# ---------------------------------------------------------------------------

test_that("recipe-level scope (cohort + tables) serializes into plan$scope", {
  recipe <- omop_recipe(
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide"),
    cohort = "scope_tbl", tables = c("inc_a", "inc_b"), combine = "intersect")

  # A string cohort here is SCOPE, not the base population's cohort.
  expect_identical(recipe$scope$cohort, "scope_tbl")
  expect_identical(recipe$scope$tables, c("inc_a", "inc_b"))
  expect_identical(recipe$scope$combine, "intersect")
  expect_null(recipe$populations$base$cohort_definition_id)

  plan <- recipe_to_plan(recipe)
  expect_identical(plan$scope$cohort, "scope_tbl")
  # tables stay a character vector: the symbols are spliced at execute time.
  expect_identical(plan$scope$tables, c("inc_a", "inc_b"))
  expect_identical(plan$scope$combine, "intersect")
})

test_that("a scalar cohort to omop_recipe is the base cohort, not scope", {
  # Back-compat: a bare cohort_definition_id keeps the legacy meaning.
  recipe <- omop_recipe(
    variables = omop_variable_age(),
    outputs = omop_output(name = "w", type = "wide"),
    cohort = 7)
  expect_identical(recipe$populations$base$cohort_definition_id, 7L)
  expect_null(recipe$scope)
})

# A fake session so .get_session() resolves and conns is non-NULL.
.with_fake_session <- function(symbol = "omop") {
  assign(symbol, structure(list(conns = list(srv = "FAKE"),
                                res_symbol = "dsO_fake"),
                           class = "omop_session"),
         envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env))
      rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env))
}

# Capture the call expression ds.omop.plan.execute() hands to
# DSI::datashield.assign.expr, with every other DSI touchpoint stubbed so no
# backend is needed.
.capture_exec_call <- function(plan, ...) {
  captured <- NULL
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, ...) {
      captured <<- expr; invisible(NULL)
    },
    datashield.aggregate = function(conns, expr, ...) list(srv = NULL),
    datashield.rm = function(conns, symbol, ...) invisible(NULL),
    .package = "DSI")
  ds.omop.plan.execute(plan, ...)
  captured
}

test_that("ds.omop.plan.execute splices scope symbols into the server call", {
  .with_fake_session()
  recipe <- omop_recipe(
    populations = list(
      omop_population("A", "A", filters = list(omop_filter_sex("F"))),
      omop_population("B", "B",
                      filters = list(omop_filter_has_concept(
                        201820, "condition_occurrence"))),
      omop_population("either", "A or B", union = c("A", "B"))),
    variables = omop_variable_age(),
    outputs = omop_output(name = "study", type = "wide",
                          population_id = "either"),
    cohort = "scope_tbl", tables = c("inc_a", "inc_b"), combine = "intersect")
  plan <- recipe_to_plan(recipe)
  # factor_concepts off so no harmonization aggregate is attempted.
  plan$options$factor_concepts <- FALSE

  call_expr <- .capture_exec_call(plan, out = c(study = "D_study"))
  expect_false(is.null(call_expr))
  expect_identical(as.character(call_expr[[1]]), "omopPlanExecuteDS")

  args <- as.list(call_expr)
  # scope and combine ride as NAMED args (so a NULL scope never shifts combine).
  expect_true("scope" %in% names(args))
  expect_true("combine" %in% names(args))
  expect_identical(args$combine, "intersect")

  # The scope expression is list(<cohort literal>, <table symbols...>): the
  # cohort travels as a string, the omop.table NAMES as bare symbols DSI resolves
  # to server-side frames (they cannot be JSON-encoded in the plan).
  scope_expr <- args$scope
  expect_identical(as.character(scope_expr[[1]]), "list")
  expect_identical(scope_expr[[2]], "scope_tbl")          # cohort literal
  expect_true(is.name(scope_expr[[3]]))                   # inc_a as a symbol
  expect_true(is.name(scope_expr[[4]]))                   # inc_b as a symbol
  expect_identical(as.character(scope_expr[[3]]), "inc_a")
  expect_identical(as.character(scope_expr[[4]]), "inc_b")

  # The plan itself crosses encoded (B64/JSON), carrying the multi-population
  # contract the server executes.
  expect_true(is.character(args[[3]]))
})

test_that("ds.omop.plan.execute omits scope args when the recipe has none", {
  .with_fake_session()
  recipe <- omop_recipe(
    variables = omop_variable_age(),
    outputs = omop_output(name = "study", type = "wide"))
  plan <- recipe_to_plan(recipe)
  plan$options$factor_concepts <- FALSE

  call_expr <- .capture_exec_call(plan, out = c(study = "D_study"))
  args <- as.list(call_expr)
  # No scope means no scope/combine args spliced in (pure positional call).
  expect_false("scope" %in% names(args))
  expect_false("combine" %in% names(args))
})

test_that("recipe_execute applies an execution-time scope override", {
  # recipe_execute(cohort=, tables=) sets the scope just before compiling, so a
  # recipe authored without scope still produces a spliced scope call.
  .with_fake_session()
  recipe <- omop_recipe(
    variables = omop_variable_age(),
    outputs = omop_output(name = "study", type = "wide"))
  expect_null(recipe$scope)

  captured <- NULL
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, ...) {
      captured <<- expr; invisible(NULL)
    },
    datashield.aggregate = function(conns, expr, ...) list(srv = NULL),
    datashield.rm = function(conns, symbol, ...) invisible(NULL),
    .package = "DSI")
  # factor harmonization is skipped because the recipe carries no factor opts by
  # default for a bare wide output; pass through recipe_execute directly.
  suppressWarnings(recipe_execute(recipe, out = c(study = "D_study"),
                                  cohort = "ct", tables = "inc_a",
                                  combine = "union"))
  expect_false(is.null(captured))
  args <- as.list(captured)
  expect_true("scope" %in% names(args))
  expect_identical(args$combine, "union")
  expect_identical(args$scope[[2]], "ct")
  expect_identical(as.character(args$scope[[3]]), "inc_a")
})

# ---------------------------------------------------------------------------
# round-trips: set-op populations + scope survive JSON/YAML and code-gen.
# ---------------------------------------------------------------------------

test_that("set-op populations + scope round-trip through YAML", {
  recipe <- omop_recipe(
    populations = list(
      omop_population("A", "A", filters = list(omop_filter_sex("F"))),
      omop_population("B", "B",
                      filters = list(omop_filter_has_concept(
                        201820, "condition_occurrence"))),
      omop_population("either", "A or B", union = c("A", "B")),
      omop_population("both", "A and B", intersect = c("A", "B"))),
    variables = omop_variable_age(),
    outputs = omop_output(name = "study", type = "wide",
                          population_id = "either"),
    cohort = "scope_tbl", tables = c("inc_a", "inc_b"), combine = "intersect")

  back <- recipe_import_yaml(recipe_export_yaml(recipe))
  expect_identical(strip_meta(recipe), strip_meta(back))
  # Specifically the set-op algebra and the scope survive.
  expect_identical(back$populations$either$setop,
                   list(op = "union", members = c("A", "B")))
  expect_identical(back$populations$both$setop,
                   list(op = "intersect", members = c("A", "B")))
  expect_identical(back$scope$tables, c("inc_a", "inc_b"))
  expect_identical(back$scope$combine, "intersect")
})

test_that("set-op populations round-trip through recipe_to_code", {
  recipe <- omop_recipe(
    populations = list(
      omop_population("A", "A", filters = list(omop_filter_sex("F"))),
      omop_population("B", "B",
                      filters = list(omop_filter_has_concept(
                        255573, "condition_occurrence"))),
      omop_population("diff", "A minus B", setdiff = c("A", "B"))),
    variables = omop_variable_age(),
    outputs = omop_output(name = "study", type = "wide",
                          population_id = "diff"))

  code <- recipe_to_code(recipe)
  rebuilt <- eval(parse(text = paste(code, collapse = "\n")))
  expect_identical(strip_meta(recipe), strip_meta(rebuilt))
  expect_identical(rebuilt$populations$diff$setop,
                   list(op = "setdiff", members = c("A", "B")))
})

# ---------------------------------------------------------------------------
# (d) single-population (base-only) recipes are byte-for-byte unchanged.
# ---------------------------------------------------------------------------

test_that("base-only recipe carries no populations/scope cruft", {
  recipe <- omop_recipe(
    variables = omop_variable_age(),
    outputs = omop_output(name = "study", type = "wide"))
  # Only the implicit base population, no scope.
  expect_identical(names(recipe$populations), "base")
  expect_null(recipe$scope)

  plan <- recipe_to_plan(recipe)
  # plan$populations carries just the base; the output targets base; no scope.
  expect_identical(names(plan$populations), "base")
  expect_identical(plan$populations$base$kind, "criteria")
  expect_identical(plan$outputs[[names(plan$outputs)[1]]]$population_id, "base")
  expect_null(plan$scope)

  # Round-trips byte-for-byte (no spurious scope key).
  expect_identical(strip_meta(recipe),
                   strip_meta(recipe_import_yaml(recipe_export_yaml(recipe))))
})
