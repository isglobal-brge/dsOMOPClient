# Pool top-K concept results using two-pass merge

Rows are matched by `key_cols`. The ranking metric and every other
additive count column in `sum_cols` are summed; a key must be released
by every server (its absence or suppression anywhere drops it, so a rare
key is never surfaced). A count suppressed on a server leaves that
pooled count `NA`. Labels such as `concept_name` are carried from the
first server that has them.

## Usage

``` r
.pool_top_k(
  per_site_dfs,
  metric_col,
  k,
  policy = "strict",
  key_cols = "concept_id",
  sum_cols = character(0)
)
```

## Arguments

  - per\_site\_dfs:
    
    Named list of data frames with the key columns and a metric column

  - metric\_col:
    
    Character; name of the metric column used for ranking

  - k:
    
    Integer; number of top results to return

  - policy:
    
    Character; pooling policy

  - key\_cols:
    
    Character; the columns identifying a row (default `"concept_id"`).

  - sum\_cols:
    
    Character; further additive count columns to sum when every server
    returns them.

## Value

List with $result (data.frame) and $warnings
