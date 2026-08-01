# Set plan-wide options on a recipe

Stores global execution options on the recipe itself, the single source
of truth. These options are applied to the compiled plan by
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md)
and therefore reach every downstream path
([`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md),
[`recipe_preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_preview.md),
[`recipe_validate`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_validate.md)).
Only non-NULL arguments are updated; existing option values are
preserved for omitted arguments. Mirrors
[`ds.omop.plan.options`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.options.md).

## Usage

``` r
recipe_set_options(
  recipe,
  translate_concepts = NULL,
  block_sensitive = NULL,
  factor_concepts = NULL
)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- translate_concepts:

  Logical; if `TRUE`, concept ID columns are translated to
  human-readable concept names in output tables.

- block_sensitive:

  Logical; if `TRUE`, sensitive columns (e.g. exact dates, free-text
  notes) are excluded from outputs.

- factor_concepts:

  Logical; if `TRUE`, after a memory-mode execution every `_concept_id`
  column is converted to a factor whose levels are harmonized across all
  connected servers.

## Value

The modified `omop_recipe` with updated options.

## See also

[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md),
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md),
[`ds.omop.plan.options`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.options.md)
