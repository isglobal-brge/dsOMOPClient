# Pool query template results across servers

Takes per-server results from
[`ds.omop.query.exec`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.query.exec.md)
and safely pools them according to the query's pool strategy. Supports
two pooling methods:

- `sum`: Sum count columns across servers (default)

- `weighted_mean`: legacy metadata value; rejected because the
  deprecated helper has no closed numerator/denominator contract

## Usage

``` r
ds.omop.query.pool(
  results,
  query_id = NULL,
  sensitive_fields = NULL,
  pool_strategy = "sum",
  policy = "strict",
  symbol = "omop"
)
```

## Arguments

- results:

  Named list of per-server data frames, as returned by
  [`ds.omop.query.exec`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.query.exec.md).

- query_id:

  Character; query ID used to look up the recommended pool strategy and
  sensitive field annotations from the query metadata. If `NULL`, the
  function falls back to auto-detection.

- sensitive_fields:

  Character vector; column names to apply suppression pooling rules to.
  If `NULL` (the default), auto-detected from query metadata or column
  name patterns.

- pool_strategy:

  Character; pooling method. `"sum"` (the default) sums count columns
  across servers; `"none"` returns the first server's result without
  pooling. The legacy `"weighted_mean"` value fails closed; use
  [`ds.omop.analysis.run()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)
  for contracted pooling.

- policy:

  Character; suppression propagation policy. `"strict"` (the default)
  sets the pooled value to NA if any server suppressed the cell.
  `"pooled_only_ok"` sums only the non-suppressed values.

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

## Value

A data frame with pooled results, or `NULL` if pooling failed (no valid
data frames, all empty, etc.). For a single server, its result is
returned as-is.

## Details

Suppression-safe pooling policy: if any server suppressed a cell (marked
as NA by the server's disclosure controls), the corresponding pooled
cell also becomes NA under `"strict"` policy. This prevents
reconstructing small-site counts by subtracting the pooled total from
known large sites.

The function auto-detects sensitive (count-like) columns from column
names matching patterns such as `n_*`, `*_count`, `count_value`, and
`num_*`. Non-sensitive columns are used as join keys for cross-server
merging.

## Deprecated

New code should use
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md),
whose `pooled` element already contains the cross-server aggregation.
This client-side pooler is retained for back-compatibility with results
produced by the deprecated
[`ds.omop.query.exec`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.query.exec.md).

## See also

[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- ds.omop.query.exec("condition_prevalence")
pooled <- ds.omop.query.pool(results, query_id = "condition_prevalence")
pooled

# Manual sensitive field specification
pooled <- ds.omop.query.pool(results,
  sensitive_fields = c("n_persons", "n_records"),
  policy = "strict")
} # }
```
