# Preview a recipe on the server

Compiles a recipe to a plan and calls `ds.omop.plan.preview`. This keeps
the plan layer available for advanced users while allowing the usual
recipe workflow to stay one-step.

## Usage

``` r
recipe_preview(recipe, symbol = "omop", conns = NULL)
```

## Arguments

  - recipe:

    An `omop_recipe` object.

  - symbol:

    Character; OMOP session symbol on the server (default `"omop"`).

  - conns:

    DSI connections or `NULL` (uses active connections).

## Value

A named list of server-side plan preview results.

## See also

`recipe_validate`, `recipe_execute`, `recipe_to_plan`

## Examples

``` r
if (FALSE) { # \dontrun{
preview <- recipe_preview(recipe)
} # }
```
