# Pool top-K concept results using two-pass merge

Pool top-K concept results using two-pass merge

## Usage

``` r
.pool_top_k(per_site_dfs, metric_col, k, policy = "strict")
```

## Arguments

  - per\_site\_dfs:

    Named list of data frames with concept\_id and a metric column

  - metric\_col:

    Character; name of the metric column to sum

  - k:

    Integer; number of top results to return

  - policy:

    Character; pooling policy

## Value

List with $result (data.frame) and $warnings
