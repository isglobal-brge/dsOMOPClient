# Add a variable block to the recipe

Registers the block and expands its `concept_ids` into individual
[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md)
objects using the block's default settings (table, format, time window,
filters). Variable names are auto-generated from concept names or IDs
with uniqueness enforcement.

## Usage

``` r
recipe_add_block(recipe, block)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- block:

  An
  [`omop_variable_block`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_block.md)
  object.

## Value

The modified `omop_recipe` object.

## See also

[`omop_variable_block`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_block.md),
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
