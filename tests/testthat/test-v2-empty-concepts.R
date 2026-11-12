# WS9M1 B2: an explicit empty ID array means none, never unrestricted concepts.
.empty_concepts_study <- function(ids, before = 30L) {
  omop_study(omop_event("drug_exposure", 1503297L), outputs = "baseline",
    baseline = list(x = omop_ever("condition_occurrence", ids, before = before)))
}

.empty_concepts_forms <- function(recipe) {
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  eval(parse(text = recipe_to_code(recipe)), envir = env)
  list(original = recipe, code = env$recipe,
    json = recipe_import_json(recipe_export_json(recipe)),
    yaml = recipe_import_yaml(recipe_export_yaml(recipe)))
}

.empty_concepts_same <- function(actual, expected) {
  actual <- recipe_to_plan(actual)
  expected <- recipe_to_plan(expected)
  expect_identical(actual, expected)
  expect_identical(dsOMOPClient:::.plan_v2_digest(actual)$sha256,
                   dsOMOPClient:::.plan_v2_digest(expected)$sha256)
}

test_that("literal, object, default and bound empty concepts round-trip without widening", {
  literal <- .empty_concepts_study(integer())
  expect_silent(dsOMOP:::.irPrepare(recipe_to_plan(literal)))
  forms <- list(literal,
    .empty_concepts_study(omop_concept_set(integer())),
    .empty_concepts_study(omop_param("ids", "concept_set", default = integer())),
    recipe_bind(.empty_concepts_study(omop_param("ids", "concept_set")), ids = integer()))
  for (recipe in forms) {
    for (roundtrip in .empty_concepts_forms(recipe)) .empty_concepts_same(roundtrip, literal)
  }
  # Prefixing must retain the empty restriction in the second namespace even
  # when the first recipe contains a nonempty concept set with the same name.
  left <- .empty_concepts_study(omop_concept_set(201826L, name = "dx"))
  expected <- recipe_merge(left,
    .empty_concepts_study(omop_concept_set(integer(), name = "dx")), prefix = "b")
  for (recipe in list(
    .empty_concepts_study(omop_concept_set(integer(), name = "dx")),
    .empty_concepts_study(omop_param("ids", "concept_set",
      default = omop_concept_set(integer(), name = "dx"))))) {
    for (roundtrip in .empty_concepts_forms(recipe_merge(left, recipe, prefix = "b"))) {
      .empty_concepts_same(roundtrip, expected)
    }
  }
})

test_that("empty concepts survive staged binding in either order and prefixed composition", {
  pending <- .empty_concepts_study(omop_param("ids", "concept_set"),
                                   omop_param("before", "integer"))
  literal <- .empty_concepts_study(integer())
  left <- .empty_concepts_study(201826L)
  for (prefixed in c(FALSE, TRUE)) {
    template <- if (prefixed) recipe_merge(left, pending, prefix = "b") else pending
    expected <- if (prefixed) recipe_merge(left, literal, prefix = "b") else literal
    for (unbound in .empty_concepts_forms(template)) {
      .empty_concepts_same(recipe_bind(unbound, ids = integer(), before = 30L), expected)
      for (first in c("ids", "before")) {
        half <- if (first == "ids") recipe_bind(unbound, ids = integer()) else
          recipe_bind(unbound, before = 30L)
        for (roundtrip in .empty_concepts_forms(half)) {
          bound <- if (first == "ids") recipe_bind(roundtrip, before = 30L) else
            recipe_bind(roundtrip, ids = integer())
          .empty_concepts_same(bound, expected)
        }
      }
    }
  }
})

test_that("typed empty ID reconstruction preserves NULL and rejects malformed ID lists", {
  ids <- dsOMOPClient:::.v2_ids
  expect_identical(ids(list(), "ids", allow_empty = TRUE), integer())
  expect_identical(ids(list(1L, 2L), "ids"), c(1L, 2L))
  expect_null(ids(NULL, "ids", allow_empty = TRUE, allow_null = TRUE))
  expect_error(ids(NULL, "ids", allow_empty = TRUE), "required")
  for (empty in list(list(), integer())) expect_error(ids(empty, "ids"), "must not be empty")
  for (bad in list(list(NULL), list(NULL, NULL), list(list()), list("bad"),
                  list(1L, "bad"), list(NA_integer_), list(-1L), list(1.5))) {
    expect_error(ids(bad, "ids", allow_empty = TRUE), "non-negative integer concept IDs")
  }
  unrestricted <- .empty_concepts_study(NULL)
  empty <- .empty_concepts_study(integer())
  expect_false(identical(recipe_to_plan(unrestricted), recipe_to_plan(empty)))
  expect_false(identical(dsOMOPClient:::.plan_v2_digest(recipe_to_plan(unrestricted))$sha256,
                         dsOMOPClient:::.plan_v2_digest(recipe_to_plan(empty))$sha256))
  for (roundtrip in .empty_concepts_forms(unrestricted)) {
    .empty_concepts_same(roundtrip, unrestricted)
    expect_null(roundtrip$variables$x$event$concepts)
  }
  pending <- .empty_concepts_study(omop_param("ids", "concept_set", default = NULL))
  for (roundtrip in .empty_concepts_forms(pending)) {
    expect_error(recipe_to_plan(roundtrip), "unbound")
    .empty_concepts_same(recipe_bind(roundtrip, ids = integer()), empty)
  }
})
