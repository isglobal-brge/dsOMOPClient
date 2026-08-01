# Get table-level statistics

Returns row counts and person counts for the specified OMOP CDM table.
Results are disclosure-controlled on the server side (counts below the
minimum cell threshold are suppressed) and returned per-site with
optional pooled totals that sum counts across servers.

## Usage

``` r
ds.omop.table.stats(
  table,
  stats = c("rows", "persons"),
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- table:

  Character; the CDM table name (e.g., `"condition_occurrence"`,
  `"person"`).

- stats:

  Character vector; which statistics to compute. Supported values are
  `"rows"` (total row count) and `"persons"` (distinct person count).
  Default: `c("rows", "persons")`.

- scope:

  Character; `"per_site"` (default) or `"pooled"`.

- pooling_policy:

  Character; `"strict"` (default) requires all servers to succeed,
  `"pooled_only_ok"` allows partial results.

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

- execute:

  Logical; if `FALSE`, return a dry-run result containing only the
  generated call code (default: `TRUE`).

## Value

A `dsomop_result` object with `$per_site` (named list of data frames
with columns `statistic` and `value`), `$pooled` (combined statistics
when pooled), and `$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ds.omop.table.stats("condition_occurrence")
result$per_site$server1

pooled <- ds.omop.table.stats("drug_exposure", scope = "pooled")
pooled$pooled
} # }
```
