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
  execute = TRUE
)
```

## Arguments

- table:

  Character; the CDM table name (e.g., `"condition_occurrence"`,
  `"drug_exposure"`).

- date_col:

  Character; the date column to aggregate by, or NULL for automatic
  detection based on the table's standard date column (default: NULL).

- granularity:

  Character; the time granularity for aggregation: `"year"` (default),
  `"quarter"`, or `"month"`.

- cohort_table:

  Character; name of a server-side cohort temp table for filtering, or
  NULL (default: NULL).

- window:

  List with `start`/`end` date strings for temporal filtering, or NULL
  (default: NULL).

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
with columns `period` and `count_value`), `$pooled` (combined counts
when pooled), and `$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
monthly <- ds.omop.date.counts("condition_occurrence",
                                granularity = "month",
                                scope = "pooled")
monthly$pooled
} # }
```
