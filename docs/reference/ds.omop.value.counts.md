# Get value frequencies for a column

Returns the top-N most frequent distinct values for a column in an OMOP
CDM table, along with their counts. This is useful for profiling
categorical or low-cardinality columns such as `type_concept_id` or
`unit_concept_id`. Counts below the disclosure threshold are suppressed,
and concept names are resolved where available.

## Usage

``` r
ds.omop.value.counts(
  table,
  column,
  top_n = 20,
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

  Character; the column to count distinct values for (e.g.,
  `"condition_type_concept_id"`).

- top_n:

  Integer; the number of most frequent values to return (default: 20).

- concept_id:

  Integer or NULL; optional concept ID to restrict rows to a single
  concept of the table before counting values (e.g., the
  `value_as_concept_id` categories for one measurement concept).
  Default: NULL for all rows. The server applies the same disclosure
  controls to the concept-filtered population.

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
with columns `value`, `count_value`), `$pooled` (combined value counts
when pooled), and `$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
vc <- ds.omop.value.counts("condition_occurrence",
                            "condition_type_concept_id",
                            top_n = 10, scope = "pooled")
vc$pooled
} # }
```
