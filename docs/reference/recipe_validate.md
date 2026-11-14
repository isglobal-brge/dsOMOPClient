# Validate a recipe on the server

Compiles a recipe to a plan and calls `ds.omop.plan.validate`.

## Usage

``` r
recipe_validate(recipe, symbol = "omop", conns = NULL)
```

## Arguments

  - recipe:

    An `omop_recipe` object.

  - symbol:

    Character; OMOP session symbol on the server (default `"omop"`).

  - conns:

    DSI connections or `NULL` (uses active connections).

## Value

A named list of server-side validation results.

## See also

`recipe_preview`, `recipe_execute`, `recipe_to_plan`

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- recipe_validate(recipe)
} # }
```
