# Remove a population from the recipe

Removes a population node from the recipe's population DAG. The base
population cannot be removed.

## Usage

``` r
recipe_remove_population(recipe, id)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- id:

  Character; population ID to remove (must not be `"base"`).

## Value

The modified `omop_recipe` object.

## See also

[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
