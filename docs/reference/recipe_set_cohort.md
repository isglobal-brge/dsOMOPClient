# Set the base cohort definition for a recipe

Updates the base population to reference an existing OMOP cohort
definition. During
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md),
this is compiled to
[`ds.omop.plan.cohort`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.cohort.md)
with `cohort_definition_id`.

## Usage

``` r
recipe_set_cohort(recipe, cohort_definition_id)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- cohort_definition_id:

  Integer or `NULL`; cohort definition ID to use as the recipe base
  population. Use `NULL` to clear the reference.

## Value

The modified `omop_recipe` object.

## See also

[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md),
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md),
[`ds.omop.plan.cohort`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.cohort.md)
