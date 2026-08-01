# Add a population to the recipe

Registers a new population node in the recipe's population DAG. The
population's parent (if any) must already exist in the recipe. The base
population is created automatically by
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md).

## Usage

``` r
recipe_add_population(recipe, population)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- population:

  An
  [`omop_population`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_population.md)
  object to add.

## Value

The modified `omop_recipe` object.

## See also

[`omop_population`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_population.md),
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
