# Preview a recipe on the server

Compiles a recipe to a plan and calls
[`ds.omop.plan.preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.preview.md).
This keeps the plan layer available for advanced users while allowing
the usual recipe workflow to stay one-step.

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

[`recipe_validate`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_validate.md),
[`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md),
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md)

## Examples

``` r
if (FALSE) { # \dontrun{
preview <- recipe_preview(recipe)
} # }
```
