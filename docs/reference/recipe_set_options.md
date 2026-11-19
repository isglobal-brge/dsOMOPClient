# Set plan-wide options on a recipe

Stores global execution options on the recipe itself, the single source
of truth. These options are applied to the compiled plan by
`recipe_to_plan` and therefore reach every downstream path
(`recipe_execute`, `recipe_preview`, `recipe_validate`). Only non-NULL
arguments are updated; existing option values are preserved for omitted
arguments. Mirrors `ds.omop.plan.options`.

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

  - translate\_concepts:
    
    Logical; chooses concept factor labels: vocabulary names when
    `TRUE`, decimal ID strings when `FALSE`. Concept columns always have
    shared factor levels across retained servers.

  - block\_sensitive:
    
    Logical; if `TRUE`, sensitive columns (e.g. exact dates, free-text
    notes) are excluded from outputs.

  - factor\_concepts:
    
    Logical; `TRUE` remains accepted. Since 2.10.6, concept IDs are
    always categories and `FALSE` is rejected.

## Value

The modified `omop_recipe` with updated options.

## See also

`omop_recipe`, `recipe_to_plan`, `ds.omop.plan.options`
