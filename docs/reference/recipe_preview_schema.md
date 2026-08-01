# Preview the output schema for a recipe

Returns the projected columns, join keys, source tables, and output
shape for each output defined in the recipe, without executing anything
on the server. Useful for verifying the recipe structure before running
it.

## Usage

``` r
recipe_preview_schema(recipe)
```

## Arguments

- recipe:

  An `omop_recipe` object.

## Value

A named list of `data.frame`s (one per output), each with columns
`output`, `column`, `source`, `concept`, `type`, and `format`.
Attributes `"join_key"`, `"tables"`, `"output_type"`, and
`"population_id"` are attached to each data.frame.

## See also

[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md),
[`recipe_preview_stats`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_preview_stats.md)

## Examples

``` r
if (FALSE) { # \dontrun{
schemas <- recipe_preview_schema(recipe)
print(schemas[["output_1"]])
} # }
```
