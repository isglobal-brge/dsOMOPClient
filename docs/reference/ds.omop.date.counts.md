# Get record counts by time period

Aggregates records in an OMOP CDM table by time period (year, quarter,
or month) and returns disclosure-safe counts. The date column is
auto-detected from the table schema if not specified. This is useful for
understanding temporal trends in data coverage and identifying gaps or
spikes in data collection.

## Usage

``` r
ds.omop.date.counts(
  table,
  date_col = NULL,
  granularity = "year",
  cohort_table = NULL,
  window = NULL,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "both",
  cohort = NULL,
  datasources = NULL
)
```

## Arguments

  - table:
    
    Character; the CDM table name (e.g., `"condition_occurrence"`,
    `"drug_exposure"`).

  - date\_col:
    
    Character; the date column to aggregate by, or NULL for automatic
    detection based on the table's standard date column (default: NULL).

  - granularity:
    
    Character; the time granularity for aggregation: `"year"` (default),
    `"quarter"`, or `"month"`.

  - cohort\_table:
    
    Character; name of a server-side cohort temp table for filtering, or
    NULL (default: NULL).

  - window:
    
    List with `start`/`end` date strings for temporal filtering, or NULL
    (default: NULL).

  - scope:
    
    Deprecated alias of `type` (`"per_site"` or `"pooled"`).

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

  - cohort:
    
    Cohort reference (a `dsomop_cohort_handle`, a
    `cohort_definition_id`, or a server-side cohort table name), or
    NULL. Takes precedence over `cohort_table`.

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry is a data frame with `period`,
`n_records` and `n_persons` (banded; periods below the threshold are
left out). `$pooled` sums the counts per period; a period missing on a
server is not treated as zero.

## Examples

``` r
if (FALSE) { # \dontrun{
monthly <- ds.omop.date.counts("condition_occurrence",
                                granularity = "month",
                                scope = "pooled")
monthly$pooled
} # }
```
