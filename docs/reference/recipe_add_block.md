# Add a variable block to the recipe

Registers the block and expands its `concept_ids` into individual
`omop_variable` objects using the block's default settings (table,
format, time window, filters). Variable names are auto-generated from
concept names or IDs with uniqueness enforcement. Adding an identical
block again leaves the recipe unchanged; a conflicting definition with
an existing block ID is refused.

## Usage

``` r
recipe_add_block(recipe, block)
```

## Arguments

  - recipe:

    An `omop_recipe` object.

  - block:

    An `omop_variable_block` object.

## Value

The modified `omop_recipe` object.

## See also

`omop_variable_block`, `omop_recipe`
