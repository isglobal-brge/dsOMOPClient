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
  conns = NULL
)
```

## Arguments

  - recipe:

    An `omop_recipe` object.

  - scope:

    Character; `"per_site"`, `"pooled"`, or `"both"`.

  - symbol:

    Character; OMOP session symbol on the server (default `"omop"`).

  - conns:

    DSI connections or `NULL` (uses active connections).

## Value

A `dsomop_result` with aggregate stats per table.

## See also

`recipe_execute`, `recipe_preview_schema`

## Examples

``` r
if (FALSE) { # \dontrun{
stats <- recipe_preview_stats(recipe, scope = "pooled")
} # }
```
