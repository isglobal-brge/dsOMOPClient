# Add an output specification to the recipe

Registers an output specification that defines how extracted data should
be shaped. Multiple outputs can target different subsets of variables
and populations, each producing a separate server-side dataset.

## Usage

``` r
recipe_add_output(recipe, output)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- output:

  An
  [`omop_output`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_output.md)
  object.

## Value

The modified `omop_recipe` object.

## See also

[`omop_output`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_output.md),
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
