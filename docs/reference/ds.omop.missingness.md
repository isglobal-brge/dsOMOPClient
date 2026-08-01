# Get missingness rates for columns

Computes the proportion and count of NULL/missing values for each column
(or a subset of columns) in an OMOP CDM table. This is essential for
data quality assessment, helping identify columns with high missingness
that may affect downstream analyses. Results are disclosure-controlled;
columns where the non-null count falls below the threshold are
suppressed.

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
  execute = TRUE
)
```

## Arguments

- table:

  Character; the CDM table name (e.g., `"measurement"`,
  `"observation"`).

- columns:

  Character vector; specific column names to check, or NULL to check all
  columns in the table (default: NULL).

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

A `dsomop_result` object with `$per_site` (named list of data frames
with columns `column_name`, `total_count`, `null_count`, `missing_pct`),
`$pooled` (combined missingness when pooled), and `$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
miss <- ds.omop.missingness("measurement",
                             columns = c("value_as_number",
                                         "value_as_concept_id"))
miss$per_site$server1
} # }
```
