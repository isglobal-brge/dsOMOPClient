# Set the base cohort definition for a recipe

Updates the base population to reference an existing OMOP cohort
definition. During `recipe_to_plan`, this is compiled to
`ds.omop.plan.cohort` with `cohort_definition_id`.

## Usage

``` r
recipe_set_cohort(recipe, cohort_definition_id)
```

## Arguments

  - recipe:

    An `omop_recipe` object.

  - cohort\_definition\_id:

    Integer or `NULL`; cohort definition ID to use as the recipe base
    population. Use `NULL` to clear the reference.

## Value

The modified `omop_recipe` object.

## See also

`omop_recipe`, `recipe_to_plan`, `ds.omop.plan.cohort`
