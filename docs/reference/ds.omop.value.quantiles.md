# Get disclosure-safe numeric quantiles

Computes quantiles for a numeric column in an OMOP CDM table. Quantile
computation happens entirely on the server side to avoid exposing
individual-level data. Quantiles are not poolable from summary
statistics: `type = "both"` returns the per-site quantiles with `pooled
= NULL` and `meta$pooling_reason`, and `type = "combine"` stops before
contacting the servers.

## Usage

``` r
ds.omop.value.quantiles(
  table,
  value_col,
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  concept_id = NULL,
  cohort_table = NULL,
  window = NULL,
  cohort = NULL,
  rounding = 2L,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "split",
  datasources = NULL
)
```

## Arguments

  - table:
    
    Character; the CDM table name (e.g., `"measurement"`).

  - value\_col:
    
    Character; the numeric column name (e.g., `"value_as_number"`).

  - probs:
    
    Numeric vector; the quantile probabilities to compute (default:
    `c(0.05, 0.25, 0.5, 0.75, 0.95)`).

  - concept\_id:
    
    Integer or NULL; optional concept ID to restrict rows to a single
    concept of the table before computing quantiles (e.g.,
    `value_as_number` for one measurement concept). Default: NULL for
    all rows. The server applies the same disclosure controls (including
    the \[0.05, 0.95\] probability clamp that blocks min/max) to the
    concept-filtered population.

  - cohort\_table:
    
    Character; name of a server-side cohort temp table for filtering, or
    NULL (default: NULL).

  - window:
    
    List with `start`/`end` date strings for temporal filtering, or NULL
    (default: NULL).

  - cohort:
    
    Cohort reference (a `dsomop_cohort_handle`, a
    `cohort_definition_id`, or a server-side cohort table name), or
    NULL. Takes precedence over `cohort_table`.

  - rounding:
    
    Integer; number of decimal places to round quantile values to
    (default: 2).

  - scope:
    
    Deprecated alias of `type` (`"per_site"` or `"pooled"`).

  - pooling\_policy:
    
    Character; `"strict"` (default) or `"pooled_only_ok"`; recorded
    only, as nothing is pooled.

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - execute:
    
    Logical; if `FALSE`, return a dry-run result containing only the
    generated call code (default: `TRUE`).

  - type:
    
    Result view: `"split"` (default) or `"both"`; see Description and
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry is the server's data frame of
probabilities and rounded quantile values; `$pooled` is always `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
q <- ds.omop.value.quantiles("measurement", "value_as_number",
                              probs = c(0.25, 0.5, 0.75))
q$per_site$server1
} # }
```
