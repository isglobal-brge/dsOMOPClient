# Get table-level statistics

Returns row counts and person counts for the specified OMOP CDM table.
Results are disclosure-controlled on the server side (counts below the
minimum cell threshold are suppressed, surviving counts are banded) and
returned per server and, by default, pooled (summed) across servers, as
`ds.dim` does.

## Usage

``` r
ds.omop.table.stats(
  table,
  stats = c("rows", "persons"),
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - table:
    
    Character; the CDM table name (e.g., `"condition_occurrence"`,
    `"person"`).

  - stats:
    
    Character vector; which statistics to compute: `"rows"` (total row
    count), `"persons"` (distinct person count) and `"date_range"`
    (first and last supported month). Default: `c("rows", "persons")`.

  - scope:
    
    Deprecated alias of `type`: `"per_site"` means `type = "split"`,
    `"pooled"` means `type = "both"`.

  - pooling\_policy:
    
    Character; `"strict"` (default) or `"pooled_only_ok"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - execute:
    
    Logical; if `FALSE`, return a dry-run result containing only the
    generated call code (default: `TRUE`).

  - type:
    
    Result view: `"both"` (default), `"split"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry is a list with the banded
`rows` and `persons` counts (a count the server suppressed is absent)
and, when requested, `date_range` (`column`, `min_month`, `max_month`).
`$pooled` has the same fields: counts summed over the servers and the
date range spanning them.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ds.omop.table.stats("condition_occurrence")
result$per_site$server1
result$pooled$persons

ds.omop.table.stats("drug_exposure", type = "split")
} # }
```
