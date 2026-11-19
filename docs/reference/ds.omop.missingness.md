# Get missingness rates for columns

Computes the proportion of NULL/missing values for each column (or a
subset of columns) in an OMOP CDM table. For person tables a person is
missing a column when none of their records carries a value. Results are
disclosure-controlled: a rate whose missing or non-missing group is
below the threshold is `NA`.

## Usage

``` r
ds.omop.missingness(
  table,
  columns = NULL,
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
    
    Character; the CDM table name (e.g., `"measurement"`,
    `"observation"`).

  - columns:
    
    Character vector; specific column names to check, or NULL to check
    all columns in the table (default: NULL).

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

A `dsomop_result`. Each `$per_site` entry is a data frame with
`column_name` and `missing_rate`; dsOMOP 2.10.9 servers add the banded
counts behind the rate, `n_total` and `n_missing`. `$pooled` has
`column_name`, `n_total`, `n_missing` and `missing_rate` = summed
`n_missing` / summed `n_total`. Older servers return rates only; their
results are not pooled and `meta$pooling_reason` says so.

## Examples

``` r
if (FALSE) { # \dontrun{
miss <- ds.omop.missingness("measurement",
                             columns = c("value_as_number",
                                         "value_as_concept_id"),
                             type = "both")
miss$pooled
} # }
```
