# Create an extraction recipe declaratively

The recipe is the central user-facing data structure for an OMOP data
extraction. It holds every selection in one place: populations (who),
variable blocks (what, grouped), individual variables, filters
(constraints), outputs (how to shape the result), the base cohort, and
plan-wide options.

## Usage

``` r
omop_recipe(
  variables = NULL,
  filters = NULL,
  outputs = NULL,
  populations = NULL,
  blocks = NULL,
  cohort = NULL,
  tables = NULL,
  combine = "union",
  options = NULL,
  output = NULL
)
```

## Arguments

- variables:

  A single
  [`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md)
  (including the convenience `omop_variable_*` derived constructors) or
  a list of them.

- filters:

  A single
  [`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md)
  /
  [`omop_filter_group`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter_group.md)
  or a list of them. A *named* list uses each name as the filter ID.

- outputs:

  A single
  [`omop_output`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_output.md)
  or a list of them.

- populations:

  A single
  [`omop_population`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_population.md)
  or a list of them (the implicit `"base"` population always exists;
  parents must be declared before their children).

- blocks:

  A single
  [`omop_variable_block`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_block.md)
  or a list of them; each block is expanded into individual variables.

- cohort:

  Recipe-level *scope*. Any form – a scalar OMOP `cohort_definition_id`,
  a cohort handle (`dsomop_cohort_handle`), or a server-side cohort
  table name – resolves to a gated person set that is intersected into
  every population (alongside `tables`). It does NOT re-root the base
  population; to build a base population from an existing cohort use
  `omop_population(cohort_definition_id = ...)`. `NULL` sets no scope.

- tables:

  Character vector of server-side `omop.table` symbol names, or `NULL`.
  Their distinct persons form a recipe-level scope folded with any
  `cohort` scope by `combine` and intersected into every population (the
  server resolves the symbol names to frames).

- combine:

  Character; how to fold multiple scope sources together: `"union"` (the
  default) or `"intersect"`.

- options:

  Named list of plan-wide options (`translate_concepts`,
  `block_sensitive`, `factor_concepts`); only supplied keys override the
  defaults.

- output:

  Convenience alias for a single
  [`omop_output`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_output.md);
  use it instead of `outputs` when the recipe has just one output.

## Value

An `omop_recipe` object.

## Details

This is the single recipe-authoring entry point: pass the complete
extraction as one nested expression built from the leaf constructors
([`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md),
[`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md),
[`omop_output`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_output.md),
[`omop_population`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_population.md),
[`omop_variable_block`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_block.md),
and friends). Each argument accepts a single object or a list of
objects. The recipe is assembled in dependency order (populations, then
blocks, then variables, then filters, then outputs, then the base
cohort, then options) so later items can reference earlier ones. Most
users then work at the recipe level with
[`recipe_preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_preview.md),
[`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md),
[`recipe_save`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_save.md),
and
[`recipe_load`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_load.md);
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md)
exposes the lower-level execution contract sent to the server.

## See also

[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md),
[`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md),
[`omop_output`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_output.md),
[`omop_population`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_population.md),
[`omop_variable_block`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_block.md),
[`recipe_preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_preview.md),
[`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md),
[`recipe_save`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_save.md),
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  blocks = omop_variable_block(
    table = "condition_occurrence",
    concept_ids = c(201820),
    format = "binary"
  ),
  variables = list(
    omop_variable_age(),
    omop_variable(table = "measurement", concept_id = 3004410,
                  format = "mean")
  ),
  filters = omop_filter_sex("F"),
  outputs = omop_output(name = "study", type = "wide")
)
recipe_execute(recipe)

# Multi-population: build two criteria subgroups, UNION them into one
# population, then run an output against that union while a recipe-level
# scope (a cohort handle INTERSECTED with a workspace omop.table's persons)
# narrows every population.
recipe2 <- omop_recipe(
  populations = list(
    omop_population("diabetic", "Diabetics",
                    filters = list(omop_filter_has_concept(
                      201820, "condition_occurrence"))),
    omop_population("hypertensive", "Hypertensives",
                    filters = list(omop_filter_has_concept(
                      320128, "condition_occurrence"))),
    omop_population("either", "Diabetic or hypertensive",
                    union = c("diabetic", "hypertensive"))
  ),
  variables = omop_variable_age(),
  outputs = omop_output(name = "study", type = "wide",
                        population_id = "either"),
  cohort = my_cohort_handle,   # cohort handle / table name as scope
  tables = "my_inclusion_set", # workspace omop.table symbol name
  combine = "intersect"
)
recipe_execute(recipe2)

# `cohort=` is ALWAYS scope, including a bare cohort_definition_id: it resolves
# to a gated person set that narrows every population. To instead BUILD the base
# population from an existing admin/ATLAS cohort, pass it through
# omop_population(cohort_definition_id = ...); `cohort=` then layers a scope on
# top.
recipe3 <- omop_recipe(
  populations = omop_population(id = "base", label = "Registry cohort",
                                cohort_definition_id = 1001),  # base population
  variables = omop_variable_age(),
  outputs = omop_output(name = "study", type = "wide"),
  cohort = 2002                                               # scalar id = scope
)
recipe_execute(recipe3)
} # }
```
