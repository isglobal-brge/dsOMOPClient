# Validate a recipe on the server

Compiles a recipe to a plan and calls
[`ds.omop.plan.validate`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.validate.md).

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

[`recipe_preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_preview.md),
[`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md),
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md)

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- recipe_validate(recipe)
} # }
```
