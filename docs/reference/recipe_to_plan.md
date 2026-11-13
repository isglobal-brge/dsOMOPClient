# Convert a recipe to an extraction plan

Compiles the recipe into the plan sent to the server. `version = 2` (the
default) emits a recipe-language 2.0 plan (`omop_plan_v2`), validated
against the vendored `plan-v2.json` schema: named concept sets, events,
intervals, populations, variables and outputs with typed references,
inline objects interned under deterministic names, legacy constructors
lowered to their native equivalents (or refused, naming the construct,
when no faithful equivalent exists). `version = 1` emits the 2.10.1 plan
unchanged for recipes built from 2.10.1 constructors; it refuses
language-2 constructs, which have no v1 equivalent.

## Usage

``` r
recipe_to_plan(recipe, version = 2L)
```

## Arguments

  - recipe:

    An `omop_recipe` object.

  - version:

    Plan version: `2` (default) or `1`.

## Value

An `omop_plan_v2` (version 2) or `omop_plan` (version 1) object ready
for execution.

## Details

`recipe_execute` chooses the version per execution: a recipe built only
from 2.10.1 constructors (and without `parent_id`) runs as its v1 plan,
so its results stay identical; any other recipe runs as a v2 plan on
servers advertising `recipe_language` 2 and every operation it needs,
and is refused (naming the operation) otherwise.

The legacy (version 1) compilation:

Compiles the recipe into an `omop_plan` suitable for server-side
execution via `ds.omop.plan.execute`. The conversion maps
population-level filters to cohort specifications, groups variables by
output and table, selects the appropriate plan builder (person\_level,
features, events, survival, intervals, baseline, temporal\_covariates,
or person\_period) for each output type, and attaches row-level filter
trees.

Multiple populations and recipe-level scope are both serialized into the
plan for the server to execute:

  - `plan$populations` carries every recipe population. A criteria
    population serializes as `list(id, label, kind = "criteria",
    filter_tree, cohort_definition_id)`; a set-op population as
    `list(id, label, kind = "setop", setop = list(op, members))`. The
    base population is always included so its cohort drives
    single-population recipes exactly as before.

  - every `plan$outputs[[name]]` carries the `population_id` it was
    authored against (default `"base"`), so the server materializes and
    gates each output against the right population.

  - `plan$scope` carries the recipe-level scope (`list(cohort, tables,
    combine)`) the server folds and intersects into every population. It
    is omitted when no scope was set.

Recipes are the recommended interface for ordinary analysis code. Plans
are retained as an explicit lower-level contract so advanced users,
tests, and the server can inspect the exact payload before it is
executed.

## See also

`recipe_execute`, `omop_recipe`, `ds.omop.plan`

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
