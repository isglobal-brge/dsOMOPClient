# ==============================================================================
# Tests for the Plan DAG builder (.recipe_to_dag / provenance)
# ==============================================================================

# Build the B1 example recipe used across the docs: condition cohort filter,
# person-derived sex + age, a measurement count variable, and a wide output.
.b1_recipe <- function() {
  recipe <- omop_recipe()
  recipe <- recipe_add_filter(recipe,
    omop_filter_has_concept(320128, "condition_occurrence"))
  recipe <- recipe_add_variable(recipe, omop_variable_sex(name = "sex"))
  recipe <- recipe_add_variable(recipe,
    omop_variable_age(name = "age", reference = "index"))
  recipe <- recipe_add_variable(recipe,
    table = "measurement", concept_id = 3022318,
    concept_name = "n_rhythm", format = "count", name = "n_rhythm")
  recipe_add_output(recipe, omop_output(name = "cohort_wide", type = "wide"))
}

test_that(".recipe_to_dag builds the expected B1 DAG structure", {
  dag <- .recipe_to_dag(.b1_recipe())

  # Cohort node present and labelled with the base population + filter tree.
  expect_true("cohort" %in% dag$nodes$id)
  cohort_lbl <- dag$nodes$label[dag$nodes$id == "cohort"]
  expect_true(grepl("All Persons", cohort_lbl))
  expect_true(grepl("condition_occurrence", cohort_lbl))

  # Person-derived sex + age: derivation nodes, no extraction nodes.
  expect_true(all(c("derive_sex", "derive_age") %in% dag$nodes$id))
  expect_false(any(c("extract_sex", "extract_age") %in% dag$nodes$id))

  # They wire straight from the cohort (skipping extraction).
  expect_equal(
    nrow(dag$edges[dag$edges$from == "cohort" & dag$edges$to == "derive_sex", ]), 1L)
  expect_equal(
    nrow(dag$edges[dag$edges$from == "cohort" & dag$edges$to == "derive_age", ]), 1L)

  # n_rhythm: cohort -> extraction -> count derivation -> output.
  expect_true("extract_n_rhythm" %in% dag$nodes$id)
  expect_true("derive_n_rhythm" %in% dag$nodes$id)
  expect_equal(
    nrow(dag$edges[dag$edges$from == "cohort" &
                     dag$edges$to == "extract_n_rhythm", ]), 1L)
  expect_equal(
    nrow(dag$edges[dag$edges$from == "extract_n_rhythm" &
                     dag$edges$to == "derive_n_rhythm", ]), 1L)

  # Output node present and wired from every variable's terminal node.
  expect_true("output_cohort_wide" %in% dag$nodes$id)
  expect_equal(
    nrow(dag$edges[dag$edges$from == "derive_n_rhythm" &
                     dag$edges$to == "output_cohort_wide", ]), 1L)
  expect_equal(
    dag$nodes$label[dag$nodes$id == "output_cohort_wide"], "cohort_wide (wide)")

  # Node groups span the four layers.
  expect_setequal(unique(dag$nodes$group),
                  c("COHORT", "DERIVATION", "EXTRACTION", "OUTPUT"))
})

test_that(".recipe_to_dag flags extraction nodes as disclosive (dashed border)", {
  dag <- .recipe_to_dag(.b1_recipe())
  ex <- dag$nodes[dag$nodes$id == "extract_n_rhythm", ]
  expect_true(ex$shapeProperties.borderDashes)
  expect_equal(ex$color.border, "#c0392b")
  # Plain derivations (count) are not distribution stats -> not flagged.
  dv <- dag$nodes[dag$nodes$id == "derive_n_rhythm", ]
  expect_false(dv$shapeProperties.borderDashes)
})

test_that(".recipe_to_dag flags cohort/output when min_persons is set", {
  recipe <- .b1_recipe()
  recipe$options$min_persons <- 5L
  dag <- .recipe_to_dag(recipe)
  expect_true(dag$nodes$shapeProperties.borderDashes[dag$nodes$id == "cohort"])
  expect_true(
    dag$nodes$shapeProperties.borderDashes[dag$nodes$id == "output_cohort_wide"])
})

test_that(".recipe_to_dag flags distribution-stat derivations (n<10)", {
  recipe <- omop_recipe()
  recipe <- recipe_add_variable(recipe,
    table = "measurement", concept_id = 3004249,
    concept_name = "sbp", format = "mean", name = "sbp_mean")
  recipe <- recipe_add_output(recipe, omop_output(name = "o", type = "wide"))
  dag <- .recipe_to_dag(recipe)
  expect_true(
    dag$nodes$shapeProperties.borderDashes[dag$nodes$id == "derive_sbp_mean"])
})

test_that(".recipe_to_dag handles NULL and empty recipes", {
  for (r in list(NULL, omop_recipe())) {
    dag <- .recipe_to_dag(r)
    expect_equal(nrow(dag$nodes), 1L)
    expect_equal(dag$nodes$id, "empty")
    expect_equal(dag$nodes$label, "No plan yet")
    expect_equal(nrow(dag$edges), 0L)
  }
})

test_that(".recipe_to_dag: raw format skips derivation", {
  recipe <- omop_recipe()
  recipe <- recipe_add_variable(recipe, table = "person",
    column = "year_of_birth", format = "raw", name = "yob")
  recipe <- recipe_add_output(recipe, omop_output(name = "o", type = "wide"))
  dag <- .recipe_to_dag(recipe)
  expect_true("extract_yob" %in% dag$nodes$id)
  expect_false("derive_yob" %in% dag$nodes$id)
  expect_equal(
    nrow(dag$edges[dag$edges$from == "extract_yob" &
                     dag$edges$to == "output_o", ]), 1L)
})

test_that(".recipe_to_dag: missing concept_name falls back to 'concept <id>'", {
  recipe <- omop_recipe()
  recipe <- recipe_add_variable(recipe, table = "measurement",
    concept_id = 9999, format = "count", name = "novar")
  dag <- .recipe_to_dag(recipe)
  expect_equal(dag$nodes$label[dag$nodes$id == "extract_novar"], "concept 9999")
})

test_that(".recipe_to_dag: multiple outputs may share one variable", {
  recipe <- omop_recipe()
  recipe <- recipe_add_variable(recipe, table = "measurement",
    concept_id = 3022318, concept_name = "shared", format = "count",
    name = "shared")
  recipe <- recipe_add_output(recipe,
    omop_output(name = "oa", type = "wide", variables = "shared"))
  recipe <- recipe_add_output(recipe,
    omop_output(name = "ob", type = "long", variables = "shared"))
  dag <- .recipe_to_dag(recipe)
  to <- dag$edges$to[dag$edges$from == "derive_shared"]
  expect_setequal(to, c("output_oa", "output_ob"))
})

test_that(".recipe_to_dag: output with no resolvable variables anchors to cohort", {
  recipe <- omop_recipe()
  recipe <- recipe_add_output(recipe,
    omop_output(name = "empty_out", type = "wide", variables = "missing"))
  dag <- .recipe_to_dag(recipe)
  expect_true("output_empty_out" %in% dag$nodes$id)
  expect_equal(
    nrow(dag$edges[dag$edges$from == "cohort" &
                     dag$edges$to == "output_empty_out", ]), 1L)
})

test_that(".render_plan_dag returns a render function (or NULL without visNetwork)", {
  ns <- shiny::NS("plan")
  rnd <- .render_plan_dag(.b1_recipe(), ns)
  if (requireNamespace("visNetwork", quietly = TRUE)) {
    expect_true(is.function(rnd))
  } else {
    expect_null(rnd)
  }
})

test_that(".render_node_provenance lists variable provenance and filters", {
  recipe <- .b1_recipe()

  html <- function(ui) paste(as.character(ui), collapse = "")

  # Extraction/derivation node -> variable provenance + cohort filter applies.
  prov <- .render_node_provenance(recipe, "extract_n_rhythm")
  h <- html(prov)
  expect_true(grepl("n_rhythm", h))
  expect_true(grepl("3022318", h))
  expect_true(grepl("measurement", h))
  expect_true(grepl("condition_occurrence", h))  # population filter applies

  # Cohort node.
  hc <- html(.render_node_provenance(recipe, "cohort"))
  expect_true(grepl("All Persons", hc))
  expect_true(grepl("condition_occurrence", hc))

  # Output node.
  ho <- html(.render_node_provenance(recipe, "output_cohort_wide"))
  expect_true(grepl("cohort_wide", ho))

  # Placeholder for no / empty selection.
  hn <- html(.render_node_provenance(recipe, NULL))
  expect_true(grepl("No node selected", hn))
  he <- html(.render_node_provenance(recipe, "empty"))
  expect_true(grepl("No node selected", he))
})
