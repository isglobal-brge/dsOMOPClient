# Disclosure-safe two-way cross-tabulation

Returns a small-cell-suppressed contingency table cross-tabulating two
categorical columns of an OMOP CDM table. The server applies primary
small-count suppression PLUS iterative complementary (secondary)
suppression to a fixpoint and never returns exact margins, so suppressed
cells cannot be recovered by row/column subtraction. Cells below the
disclosure threshold render as `NA`; structural zeros render as `0`.
Concept-id axes are decorated with concept names.

When `stratify_by` is supplied, the result is a named list of
independent protected 2-way slices (one per stratum level) rather than a
single table; the unstratified total is never returned.

## Usage

``` r
ds.omop.crosstab(
  table,
  row,
  col,
  by = "persons",
  row_concept_id = NULL,
  col_concept_id = NULL,
  cohort_table = NULL,
  stratify_by = NULL,
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

  Character; the CDM table name (e.g., `"person"`).

- row:

  Character; the column for table rows (e.g., `"gender_concept_id"`).

- col:

  Character; the column for table columns (e.g., `"race_concept_id"`).

- by:

  Character; `"persons"` (default, distinct person counts) or
  `"records"` (row counts).

- row_concept_id:

  Integer/vector or NULL; optional concept ID(s) to restrict the row
  axis. Default: NULL.

- col_concept_id:

  Integer/vector or NULL; optional concept ID(s) to restrict the column
  axis. Default: NULL.

- cohort_table:

  Character or NULL; optional cohort table to scope the population
  (inner join on `subject_id`). Default: NULL.

- stratify_by:

  Character or NULL; optional third categorical column to produce a
  named list of independent stratified 2-way slices. Default: NULL.

- cohort:

  Cohort reference (a `dsomop_cohort_handle`, a `cohort_definition_id`,
  or a server-side cohort table name), or NULL. Takes precedence over
  `cohort_table`.

- scope:

  Character; `"per_site"` (default) or `"pooled"`.

- pooling_policy:

  Character; `"strict"` (default) or `"pooled_only_ok"`. Under `strict`,
  a cell absent or suppressed on any site is suppressed in the pooled
  table.

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

- execute:

  Logical; if `FALSE`, return a dry-run result containing only the
  generated call code (default: `TRUE`).

## Value

A `dsomop_result` object with `$per_site` (named list of server
cross-tab objects), `$pooled` (cell-wise summed table when pooled), and
`$meta`.

## Cross-tab is descriptive, not inferential

A cross-tab answers "how do these two variables co-occur?". It is NOT a
substitute for a multivariable model. For genuine multivariable
questions (three or more interacting variables, continuous adjustment,
or estimating an association while controlling for confounders), route
to [`ds.glm`](https://rdrr.io/pkg/dsBaseClient/man/ds.glm.html) instead
of building higher-dimensional cell tables, which are disclosure-unsafe
and lose utility on small data.

## Examples

``` r
if (FALSE) { # \dontrun{
ct <- ds.omop.crosstab("person", "gender_concept_id", "race_concept_id",
                       scope = "pooled")
ct$pooled$counts
} # }
```
