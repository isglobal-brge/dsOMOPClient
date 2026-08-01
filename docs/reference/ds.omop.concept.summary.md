# Summarise a value column scoped to one concept of one table

Type-aware orchestrator that summarises a value column for a single
concept of a single OMOP CDM table. The unit of analysis is the
`(table, concept_id, column)` triple: in OMOP a value column only makes
sense within a concept (a `measurement` table mixes HbA1c, weight, blood
pressure, ...), so restricting to one concept yields an interpretable
distribution. Numeric and categorical value columns receive different
DataSHIELD-safe statistics:

- A `*_concept_id` value column (e.g. `value_as_concept_id`) is treated
  as CATEGORICAL and summarised with
  [`ds.omop.value.counts`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.value.counts.md)
  (disclosure-safe frequency counts of the categories observed for this
  concept).

- `value_as_number` (or any numeric value column) is treated as NUMERIC
  and summarised with both
  [`ds.omop.column.stats`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.column.stats.md)
  (n, mean, SD, missingness, distinct count) and
  [`ds.omop.value.quantiles`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.value.quantiles.md)
  (median, IQR, percentiles). Min/max are never returned: the server
  clamps quantile probabilities to \[0.05, 0.95\].

This function only adds a `concept_id` filter to queries that are
already disclosure-gated server-side; the existing gates fire on the
concept-filtered population, so a concept with too few persons is
blocked.

## Usage

``` r
ds.omop.concept.summary(
  table,
  concept_id,
  column = NULL,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- table:

  Character; the CDM table name (e.g., `"measurement"`,
  `"observation"`).

- concept_id:

  Integer; the OMOP concept ID to scope the value column(s) to (e.g.,
  the HbA1c measurement concept).

- column:

  Character or NULL; a single value column to summarise. If NULL
  (default), the table's columns are fetched via
  [`ds.omop.columns`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.columns.md)
  and whichever of `c("value_as_number", "value_as_concept_id")` are
  present are summarised.

- scope:

  Character; `"per_site"` (default) or `"pooled"`. Passed through to the
  underlying calls.

- pooling_policy:

  Character; `"strict"` (default) or `"pooled_only_ok"` (Best Effort).
  Passed through to the underlying value-counts, column-stats and
  quantile calls so that, when pooled, categories/values present on only
  some sites are summed across the available sites rather than
  suppressed.

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

## Value

A named list with `table`, `concept_id`, `numeric` (named-by-column list
where each element is a list with `stats` and `quantiles`
`dsomop_result` objects, or NULL when no numeric value column applies),
and `categorical` (named-by-column list of `dsomop_result` objects from
value counts, or NULL when no categorical value column applies).

## Examples

``` r
if (FALSE) { # \dontrun{
# Distribution of value_as_number for an HbA1c measurement concept
summ <- ds.omop.concept.summary("measurement", concept_id = 3004410)
summ$numeric$value_as_number$stats$per_site$server1
summ$numeric$value_as_number$quantiles$per_site$server1

# Categorical value_as_concept_id breakdown for an observation concept
obs <- ds.omop.concept.summary("observation", concept_id = 4058243,
                                column = "value_as_concept_id")
obs$categorical$value_as_concept_id$per_site$server1
} # }
```
