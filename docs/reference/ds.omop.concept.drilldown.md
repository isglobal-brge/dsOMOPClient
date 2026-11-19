# Get concept drilldown profile

Returns a comprehensive drilldown profile for a single concept within an
OMOP CDM table. The profile includes summary statistics (record count,
person count), numeric distribution (if applicable), categorical value
breakdown, date coverage range, and missingness rates for associated
columns. All results are disclosure-controlled on the server side before
being returned.

## Usage

``` r
ds.omop.concept.drilldown(
  table,
  concept_id,
  concept_col = NULL,
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
    
    Character; the CDM table name (e.g., `"condition_occurrence"`,
    `"measurement"`).

  - concept\_id:
    
    Integer; the OMOP concept ID to drill into.

  - concept\_col:
    
    Character; the concept column to drill into, or NULL for automatic
    detection based on the table's standard concept column (default:
    NULL).

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
    
    Result view: `"split"` (default), `"both"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry is a list with `summary`
(`concept_id`, `concept_name`, banded `n_records` and `n_persons`,
`records_per_person_mean`, `pct_persons_multi`), `numeric_summary`
(`quantiles`, `histogram`), `categorical_values` (`value_as_concept_id`,
`concept_name`, `n`), `date_range` (`column`, `min_month_safe`,
`max_month_safe`, yearly `date_counts`) and `missingness`
(`column_name`, `missing_rate`). `$pooled` has the same parts: counts,
histogram bins, categorical values and yearly counts summed, the date
range spanned, and the non-additive parts (quantiles,
`pct_persons_multi`, and missing rates the servers return without
counts) left `NULL` or `NA` with a warning.

## Examples

``` r
if (FALSE) { # \dontrun{
profile <- ds.omop.concept.drilldown("condition_occurrence",
                                      concept_id = 201820)
profile$per_site$server1$summary
profile$per_site$server1$numeric
} # }
```
