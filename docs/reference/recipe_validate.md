# Validate a recipe on the server

Compiles a recipe to a plan and calls `ds.omop.plan.validate`.

## Usage

``` r
recipe_validate(
  recipe,
  symbol = "omop",
  conns = NULL,
  type = "split",
  datasources = NULL
)
```

## Arguments

  - recipe:
    
    An `omop_recipe` object.

  - symbol:
    
    Character; OMOP session symbol on the server (default `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"split"` (default) or `"both"`; see
    `ds.omop.plan.validate`.

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result` from `ds.omop.plan.validate`: its `$per_site` is the
named list of server-side validation results returned by earlier
versions, and `res$<server>` still reaches one server's result.

## See also

`recipe_preview`, `recipe_execute`, `recipe_to_plan`

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- recipe_validate(recipe)
} # }
```
