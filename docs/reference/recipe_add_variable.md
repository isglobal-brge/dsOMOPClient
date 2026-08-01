# Add a variable to the recipe

Adds a single variable to the recipe. Either pass a pre-built
[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md)
object, or pass arguments directly which will be forwarded to
[`omop_variable()`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md).
The variable name is deduplicated if it conflicts with existing names.

## Usage

``` r
recipe_add_variable(recipe, variable = NULL, ...)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- variable:

  An
  [`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md)
  object, or `NULL` to construct from `...`.

- ...:

  If `variable` is `NULL`, arguments passed to
  [`omop_variable()`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md).

## Value

The modified `omop_recipe` object.

## See also

[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md),
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
