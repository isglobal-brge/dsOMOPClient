# Add a variable to the recipe

Adds a single variable to the recipe. Either pass a pre-built
`omop_variable` object, or pass arguments directly which will be
forwarded to `omop_variable()`. The variable name is deduplicated if it
conflicts with existing names.

## Usage

``` r
recipe_add_variable(recipe, variable = NULL, ...)
```

## Arguments

  - recipe:

    An `omop_recipe` object.

  - variable:

    An `omop_variable` object, or `NULL` to construct from `...`.

  - ...:

    If `variable` is `NULL`, arguments passed to `omop_variable()`.

## Value

The modified `omop_recipe` object.

## See also

`omop_variable`, `omop_recipe`
