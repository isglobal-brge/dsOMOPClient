# Pool query template results across servers

Takes per-server results from `ds.omop.query.exec` and delegates to the
exact server-owned pooling contract carried by that result. It never
infers keys, count fields, rates, or statistics from column names.

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
    `ds.omop.query.exec`.

  - query\_id:

    Optional character query ID; when supplied, it must match the
    analysis identity carried by `results`.

  - sensitive\_fields:

    Deprecated and unsupported; must remain `NULL`.

  - pool\_strategy:

    Deprecated and unsupported; must remain `"sum"`.

  - policy:

    Character; suppression propagation policy. `"strict"` (the default)
    sets the pooled value to NA if any server suppressed the cell.
    `"pooled_only_ok"` sums only the non-suppressed values.

  - symbol:

    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

## Value

A data frame with contractually pooled results, or `NULL` when the
contracted estimand has no combinable result.

## Details

Suppression-safe pooling policy: if any server suppressed a cell (marked
as NA by the server's disclosure controls), the corresponding pooled
cell also becomes NA under `"strict"` policy. This prevents
reconstructing small-site counts by subtracting the pooled total from
known large sites.

## Deprecated

New code should use `ds.omop.analysis.run`, whose `pooled` element
already contains the cross-server aggregation. This helper remains only
as a contractual bridge for results from deprecated
`ds.omop.query.exec`.

## See also

`ds.omop.analysis.run`

## Examples

``` r
if (FALSE) { # \dontrun{
results <- ds.omop.query.exec("condition_prevalence")
pooled <- ds.omop.query.pool(results, query_id = "condition_prevalence")
pooled
} # }
```
