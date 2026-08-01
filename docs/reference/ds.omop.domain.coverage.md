# Get cross-table domain coverage

Provides a high-level overview of data coverage across all OMOP CDM
domain tables. For each clinical domain (conditions, drugs, procedures,
measurements, etc.), returns row counts, person counts, and date ranges.
This is useful for quickly assessing which data domains are populated
and to what extent, without needing to query each table individually.

## Usage

``` r
ds.omop.domain.coverage(
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- scope:

  Character; `"per_site"` (default) or `"pooled"`.

- pooling_policy:

  Character; `"strict"` (default) or `"pooled_only_ok"`.

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

- execute:

  Logical; if `FALSE`, return a dry-run result containing only the
  generated call code (default: `TRUE`).

## Value

A `dsomop_result` object with `$per_site` (named list of data frames
with columns `table_name`, `row_count`, `person_count`), `$pooled`
(combined coverage when pooled), and `$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
coverage <- ds.omop.domain.coverage(scope = "pooled")
coverage$pooled
} # }
```
