# Add a filter to the recipe

Registers a filter or filter group in the recipe. Filters are applied
during plan compilation: population-level filters restrict the cohort,
row-level filters restrict events. The filter ID is auto-generated from
the type if not provided.

## Usage

``` r
recipe_add_filter(recipe, filter, id = NULL)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- filter:

  An
  [`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md)
  or
  [`omop_filter_group`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter_group.md)
  object.

- id:

  Character or `NULL`; filter ID (auto-generated from type and sequence
  number if `NULL`).

## Value

The modified `omop_recipe` object.

## See also

[`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md),
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
