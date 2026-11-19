# Get concept prevalence for a table

Retrieves the most frequent concepts in the specified OMOP CDM table,
ranked by person count or record count. Results are
disclosure-controlled on the server side (small cells are suppressed)
and returned as a `dsomop_result` with per-site and optionally pooled
data. Pooling sums counts across servers and re-ranks.

## Usage

``` r
ds.omop.concept.prevalence(
  table = NULL,
  concept_col = NULL,
  metric = "persons",
  top_n = 50,
  cohort_table = NULL,
  window = NULL,
  offset = 0L,
  global = FALSE,
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
    
    Character; the CDM table name (e.g., `"condition_occurrence"`,
    `"drug_exposure"`).

  - concept\_col:
    
    Character; the concept column name, or NULL for automatic detection
    based on the table's standard concept column (default: NULL).

  - metric:
    
    Character; `"persons"` (default) to rank by distinct person count,
    or `"records"` to rank by total record count.

  - top\_n:
    
    Integer; number of top concepts to return (default: 50).

  - cohort\_table:
    
    Character; name of a server-side cohort temp table to restrict the
    analysis to a specific cohort (default: NULL).

  - window:
    
    List with `start` and `end` date strings (ISO 8601) for temporal
    filtering, or NULL for no date restriction (default: NULL).

  - offset:
    
    Integer; number of ranked concepts to skip for pagination (default:
    0).

  - global:
    
    Logical; if `TRUE`, rank concepts across all supported clinical
    tables rather than only `table` (default: `FALSE`).

  - cohort:
    
    Cohort reference (a `dsomop_cohort_handle`, a
    `cohort_definition_id`, or a server-side cohort table name), or
    NULL. Takes precedence over `cohort_table`.

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
`concept_id`, `concept_name`, `n_persons` and `n_records` (banded), plus
`source_table` for a global ranking. `$pooled` sums `n_persons` and
`n_records` per concept (and source table) over the servers, ranks by
`metric` and keeps the first `top_n`; a concept must be released by
every server.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ds.omop.concept.prevalence("condition_occurrence")
head(result$per_site$server1)

pooled <- ds.omop.concept.prevalence("drug_exposure",
                                      metric = "records",
                                      scope = "pooled")
pooled$pooled
} # }
```
