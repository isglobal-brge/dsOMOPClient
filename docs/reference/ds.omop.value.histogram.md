# Get a disclosure-safe numeric histogram

Computes a binned histogram for a numeric column in an OMOP CDM table.
When scope is `"pooled"`, a two-pass algorithm is used: the first pass
collects p05/p95 ranges from each server to compute shared bin edges,
and the second pass counts records per bin using those shared edges so
that results are directly comparable and summable across servers. Bins
with counts below the disclosure threshold are suppressed.

## Usage

``` r
ds.omop.value.histogram(
  table,
  value_col,
  bins = 20L,
  concept_id = NULL,
  cohort_table = NULL,
  window = NULL,
  cohort = NULL,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  plot = FALSE,
  nbins = 9L,
  xlab = NULL,
  main = NULL,
  col = "#4C72B0"
)
```

## Arguments

- table:

  Character; the CDM table name (e.g., `"measurement"`).

- value_col:

  Character; the numeric column to histogram (e.g.,
  `"value_as_number"`).

- bins:

  Integer; the number of histogram bins (default: 20).

- concept_id:

  Integer or NULL; optional concept ID to restrict rows to a single
  concept of the table before binning (e.g. `value_as_number` for one
  measurement concept). Default: NULL for all rows. The server applies
  the same disclosure controls to the concept-filtered population.
  Requires a dsOMOP server build with histogram concept scoping; older
  servers reject the argument (use the concept-scoped quantiles
  aggregate via
  [`ds.omop.value.quantiles()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.value.quantiles.md)
  as a fallback).

- cohort_table:

  Character; name of a server-side cohort temp table for filtering, or
  NULL (default: NULL).

- window:

  List with `start`/`end` date strings for temporal filtering, or NULL
  (default: NULL).

- cohort:

  Cohort reference (a `dsomop_cohort_handle`, a `cohort_definition_id`,
  or a server-side cohort table name), or NULL. Takes precedence over
  `cohort_table`.

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

- plot:

  Logical; if `TRUE`, draw a federation-wide bar chart of the pooled,
  shared-edge bins (forces `scope = "pooled"`) and return the result
  invisibly. Default `FALSE`.

- nbins:

  Integer; number of display bins used when `plot = TRUE` (default: 9).

- xlab, main, col:

  Axis label, title and bar colour used when `plot = TRUE`.

## Value

A `dsomop_result` object with `$per_site` (named list of data frames
with columns `bin_start`, `bin_end`, `count_value`), `$pooled` (combined
histogram when pooled), and `$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
hist_result <- ds.omop.value.histogram("measurement", "value_as_number",
                                        bins = 30, scope = "pooled")
hist_result$pooled
} # }
```
