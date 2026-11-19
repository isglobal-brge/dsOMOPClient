# Locate concept across all CDM tables

Searches all OMOP CDM tables that contain concept columns and returns a
presence matrix showing which tables contain the specified concept IDs.
This is useful for understanding where a concept appears in the database
before performing deeper exploration or extraction. Counts are
disclosure-controlled; tables where a concept appears fewer than the
threshold number of times are reported as suppressed.

## Usage

``` r
ds.omop.concept.locate(
  concept_ids,
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

  - concept\_ids:
    
    Integer vector; one or more OMOP concept IDs to search for across
    all CDM tables.

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

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry is a data frame with
`table_name`, `concept_column`, `concept_id`, `n_records` and
`n_persons` (banded; tables below the threshold are left out). `$pooled`
sums the counts per concept, table and column.

## Examples

``` r
if (FALSE) { # \dontrun{
loc <- ds.omop.concept.locate(c(201820, 316139))
loc$per_site$server1
} # }
```
