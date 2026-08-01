# Convert a recipe to an extraction plan

Compiles the recipe into an `omop_plan` suitable for server-side
execution via
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md).
The conversion maps population-level filters to cohort specifications,
groups variables by output and table, selects the appropriate plan
builder (person_level, features, events, survival, intervals, baseline,
temporal_covariates, or person_period) for each output type, and
attaches row-level filter trees.

## Usage

``` r
recipe_to_plan(recipe)
```

## Arguments

- recipe:

  An `omop_recipe` object.

## Value

An `omop_plan` object ready for execution.

## Details

Multiple populations and recipe-level scope are both serialized into the
plan for the server to execute:

- `plan$populations` carries every recipe population. A criteria
  population serializes as
  `list(id, label, kind = "criteria", filter_tree, cohort_definition_id)`;
  a set-op population as
  `list(id, label, kind = "setop", setop = list(op, members))`. The base
  population is always included so its cohort drives single-population
  recipes exactly as before.

- every `plan$outputs[[name]]` carries the `population_id` it was
  authored against (default `"base"`), so the server materializes and
  gates each output against the right population.

- `plan$scope` carries the recipe-level scope
  (`list(cohort, tables, combine)`) the server folds and intersects into
  every population. It is omitted when no scope was set.

Recipes are the recommended interface for ordinary analysis code. Plans
are retained as an explicit lower-level contract so advanced users,
tests, and the server can inspect the exact payload before it is
executed.

## See also

[`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md),
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md),
[`ds.omop.plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  blocks = omop_variable_block(
    table = "condition_occurrence",
    concept_ids = c(201820), format = "binary"),
  outputs = omop_output(type = "wide"))
recipe_execute(recipe)
plan <- recipe_to_plan(recipe)  # advanced: inspect the server payload
} # }
```
