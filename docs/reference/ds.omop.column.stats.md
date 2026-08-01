# Get column-level statistics

Returns detailed statistics for a single column in an OMOP CDM table,
including data type, non-null count, distinct count, and (for numeric
columns) min, max, mean, and standard deviation. All counts are
disclosure-controlled on the server side.

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
  execute = TRUE
)
```

## Arguments

- table:

  Character; the CDM table name (e.g., `"condition_occurrence"`).

- column:

  Character; the column name to profile (e.g., `"condition_start_date"`,
  `"value_as_number"`).

- concept_id:

  Integer or NULL; optional concept ID to restrict rows to a single
  concept of the table before computing the column statistics (e.g.,
  `value_as_number` for one measurement concept). Default: NULL for all
  rows. The server applies the same disclosure controls to the
  concept-filtered population.

- cohort:

  Cohort reference (a `dsomop_cohort_handle`, a `cohort_definition_id`,
  or a server-side cohort table name), or NULL.

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

A `dsomop_result` object with `$per_site` (named list of data frames or
lists with column-level statistics), `$pooled` (combined statistics when
pooled), and `$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
col_info <- ds.omop.column.stats("measurement", "value_as_number")
col_info$per_site$server1
} # }
```
