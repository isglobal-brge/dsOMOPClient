# Preview aggregate stats for a recipe (without materializing)

Runs aggregate-only queries via `ds.omop.table.stats` to show safe row
and person counts for the tables referenced by the recipe's variables.
This provides a quick sanity check without materializing any server-side
datasets.

## Usage

``` r
recipe_preview_stats(
  recipe,
  scope = c("per_site", "pooled", "both"),
  symbol = "omop",
  conns = NULL,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - recipe:
    
    An `omop_recipe` object.

  - scope:
    
    Deprecated alias of `type`: `"per_site"` means `"split"`; `"pooled"`
    and `"both"` mean `"both"`.

  - symbol:
    
    Character; OMOP session symbol on the server (default `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"both"` (default), `"split"`, or `"combine"`, as in
    `ds.omop.table.stats`.

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry is one server's data frame
with one row per table referenced by the recipe (`table`, banded `rows`
and `persons`; `NA` where the server suppressed or did not return a
count). `$pooled` has the same columns with the counts summed over the
servers. The per-table `ds.omop.table.stats()` results of earlier
versions are kept in `meta$by_table`.

## See also

`recipe_execute`, `recipe_preview_schema`

## Examples

``` r
if (FALSE) { # \dontrun{
stats <- recipe_preview_stats(recipe)
stats$pooled
} # }
```
