# Pool top-K concept results using two-pass merge

Pool top-K concept results using two-pass merge

## Usage

``` r
.pool_top_k(per_site_dfs, metric_col, k, policy = "strict")
```

## Arguments

- per_site_dfs:

  Named list of data frames with concept_id and a metric column

- metric_col:

  Character; name of the metric column to sum

- k:

  Integer; number of top results to return

- policy:

  Character; pooling policy

## Value

List with \$result (data.frame) and \$warnings
