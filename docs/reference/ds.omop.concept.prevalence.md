# Get concept prevalence for a table

Retrieves the most frequent concepts in the specified OMOP CDM table,
ranked by person count or record count. Results are
disclosure-controlled on the server side (small cells are suppressed)
and returned as a `dsomop_result` with per-site and optionally pooled
data. Pooling sums counts across servers and re-ranks.

## Usage

``` r
ds.omop.concept.prevalence(
  table = NULL,
  concept_col = NULL,
  metric = "persons",
  top_n = 50,
  cohort_table = NULL,
  window = NULL,
  offset = 0L,
  global = FALSE,
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

  Character; the CDM table name (e.g., `"condition_occurrence"`,
  `"drug_exposure"`).

- concept_col:

  Character; the concept column name, or NULL for automatic detection
  based on the table's standard concept column (default: NULL).

- metric:

  Character; `"persons"` (default) to rank by distinct person count, or
  `"records"` to rank by total record count.

- top_n:

  Integer; number of top concepts to return (default: 50).

- cohort_table:

  Character; name of a server-side cohort temp table to restrict the
  analysis to a specific cohort (default: NULL).

- window:

  List with `start` and `end` date strings (ISO 8601) for temporal
  filtering, or NULL for no date restriction (default: NULL).

- offset:

  Integer; number of ranked concepts to skip for pagination (default:
  0).

- global:

  Logical; if `TRUE`, rank concepts across all supported clinical tables
  rather than only `table` (default: `FALSE`).

- cohort:

  Cohort reference (a `dsomop_cohort_handle`, a `cohort_definition_id`,
  or a server-side cohort table name), or NULL. Takes precedence over
  `cohort_table`.

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
with columns `concept_id`, `concept_name`, `count_value`, etc.),
`$pooled` (combined data frame when scope is `"pooled"`, otherwise
NULL), and `$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ds.omop.concept.prevalence("condition_occurrence")
head(result$per_site$server1)

pooled <- ds.omop.concept.prevalence("drug_exposure",
                                      metric = "records",
                                      scope = "pooled")
pooled$pooled
} # }
```
