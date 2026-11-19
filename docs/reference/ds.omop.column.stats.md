# Get column-level statistics

Returns statistics for a single column in an OMOP CDM table: banded row,
person and missing counts, the number of distinct values, and (for
numeric columns) the mean and standard deviation of one value per
person. All counts are disclosure-controlled on the server side. Like
`ds.mean`, the default view is per server.

## Usage

``` r
ds.omop.column.stats(
  table,
  column,
  concept_id = NULL,
  cohort = NULL,
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
    
    Character; the CDM table name (e.g., `"condition_occurrence"`).

  - column:
    
    Character; the column name to profile (e.g.,
    `"condition_start_date"`, `"value_as_number"`).

  - concept\_id:
    
    Integer or NULL; optional concept ID to restrict rows to a single
    concept of the table before computing the column statistics (e.g.,
    `value_as_number` for one measurement concept). Default: NULL for
    all rows. The server applies the same disclosure controls to the
    concept-filtered population.

  - cohort:
    
    Cohort reference (a `dsomop_cohort_handle`, a
    `cohort_definition_id`, or a server-side cohort table name), or
    NULL.

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
    
    Result view: `"split"` (default), `"both"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry is a list with `n_total`,
`n_missing`, `n_distinct` and, for person tables, `n_persons` (counts
banded; a suppressed value is `NA`), plus `mean` and `sd` for numeric
columns. `$pooled` sums the counts, weights the mean by the contributing
persons and pools the SD; the distinct count is not additive and is not
pooled.

## Examples

``` r
if (FALSE) { # \dontrun{
col_info <- ds.omop.column.stats("measurement", "value_as_number")
col_info$per_site$server1
} # }
```
