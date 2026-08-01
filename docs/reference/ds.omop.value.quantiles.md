# Get disclosure-safe numeric quantiles

Computes quantiles for a numeric column in an OMOP CDM table. Quantile
computation happens entirely on the server side to avoid exposing
individual-level data. Note that quantiles are inherently non-poolable
from summary statistics alone; when `scope = "pooled"` is requested,
per-site quantiles are still returned but a warning is emitted and the
`$pooled` slot remains NULL.

## Usage

``` r
ds.omop.value.quantiles(
  table,
  value_col,
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  concept_id = NULL,
  cohort_table = NULL,
  window = NULL,
  cohort = NULL,
  rounding = 2L,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- table:

  Character; the CDM table name (e.g., `"measurement"`).

- value_col:

  Character; the numeric column name (e.g., `"value_as_number"`).

- probs:

  Numeric vector; the quantile probabilities to compute (default:
  `c(0.05, 0.25, 0.5, 0.75, 0.95)`).

- concept_id:

  Integer or NULL; optional concept ID to restrict rows to a single
  concept of the table before computing quantiles (e.g.,
  `value_as_number` for one measurement concept). Default: NULL for all
  rows. The server applies the same disclosure controls (including the
  \[0.05, 0.95\] probability clamp that blocks min/max) to the
  concept-filtered population.

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

- rounding:

  Integer; number of decimal places to round quantile values to
  (default: 2).

- scope:

  Character; `"per_site"` (default) or `"pooled"`. Pooled quantiles are
  not computed (see Description).

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

A `dsomop_result` object with `$per_site` (named list of named numeric
vectors or data frames with quantile values), `$pooled` (always NULL
since quantiles cannot be safely pooled), and `$meta` (includes warnings
when pooled scope is requested).

## Examples

``` r
if (FALSE) { # \dontrun{
q <- ds.omop.value.quantiles("measurement", "value_as_number",
                              probs = c(0.25, 0.5, 0.75))
q$per_site$server1
} # }
```
