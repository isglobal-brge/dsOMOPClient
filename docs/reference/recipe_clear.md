# Clear the entire recipe

Discards all populations, blocks, variables, filters, and outputs,
returning a fresh empty recipe identical to
[`omop_recipe()`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md).

## Usage

``` r
recipe_clear(recipe)
```

## Arguments

- recipe:

  An `omop_recipe` object (used only for type validation).

## Value

A new empty `omop_recipe` object.

## See also

[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
