# Get safe numeric cutpoints for a column

Returns a public numeric grid configured by the data controller for an
OMOP column. The server releases the complete grid only when every bin
is supported by the minimum number of distinct persons after
one-contribution- per-person reduction. Edges are not estimated from
protected values.

## Usage

``` r
ds.omop.safe.cutpoints(
  table,
  column,
  concept_id = NULL,
  n_bins = 10L,
  scope = c("per_site", "pooled"),
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "split",
  datasources = NULL
)
```

## Arguments

  - table:
    
    Character; the OMOP CDM table name (e.g., `"measurement"`,
    `"observation"`).

  - column:
    
    Character; the numeric column to bin (e.g., `"value_as_number"`).

  - concept\_id:
    
    Integer or NULL; optional concept ID to restrict rows before
    computing bins (default: NULL for all rows).

  - n\_bins:
    
    Integer; exact number of bins in the controller-configured public
    grid (default: 10). Unsupported or under-populated grids fail
    closed; bins are never merged based on protected counts.

  - scope:
    
    Deprecated alias of `type` (`"per_site"` or `"pooled"`).

  - symbol:
    
    Character; the session symbol identifying the OMOP connection
    (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - execute:
    
    Logical; if `FALSE`, return a dry-run result containing only the
    generated call code (default: `TRUE`).

  - type:
    
    Result view: `"split"` (default) or `"both"`. Cutpoints are
    site-specific: `"both"` returns `pooled = NULL` with
    `meta$pooling_reason`, and `"combine"` stops before contacting the
    servers. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result` object with `$per_site` (named list where each element
contains public `breaks`, banded `counts`, a session `contract`, and
clipping/grid metadata), `$pooled` (always NULL for cutpoints), and
`$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
cuts <- ds.omop.safe.cutpoints("measurement", "value_as_number",
                                concept_id = 3004249, n_bins = 5)
cuts$per_site$server1$breaks
} # }
```
