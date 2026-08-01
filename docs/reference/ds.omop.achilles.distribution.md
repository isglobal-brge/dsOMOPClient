# Get Achilles distribution results

Queries the `achilles_results_dist` table on each connected server for
the specified distribution analysis IDs (e.g., age distribution,
observation period length). Distribution analyses store percentile
values (p10, p25, median, p75, p90) rather than raw counts. Extreme
values (min/max) are never returned by the server for disclosure
protection.

## Usage

``` r
ds.omop.achilles.distribution(
  analysis_ids,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- analysis_ids:

  Integer vector; distribution analysis IDs to retrieve (e.g.,
  `c(103, 105)` for age and observation length distributions).

- scope:

  Character; `"per_site"` returns each server's results separately,
  `"pooled"` additionally attempts weighted aggregation.

- pooling_policy:

  Character; controls how suppressed (NA) cells are handled during
  pooling. `"strict"` (the default) or `"pooled_only_ok"`.

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

- execute:

  Logical; if `FALSE`, returns a dry-run `dsomop_result` containing only
  the reproducible R code without contacting the servers.

## Value

A `dsomop_result` object. Per-site results contain distribution data
frames with columns `analysis_id`, `stratum_1`, `count_value`,
`avg_value`, `stdev_value`, `p10_value`, `p25_value`, `median_value`,
`p75_value`, `p90_value`.

## Details

When `scope = "pooled"`, pooling is attempted via weighted aggregation;
however, percentile values cannot be meaningfully combined from
pre-computed summaries, so all percentile columns in the pooled result
are set to `NA`. Use per-site data for percentile display.

## Examples

``` r
if (FALSE) { # \dontrun{
# Age distribution (analysis 103) per site
age_dist <- ds.omop.achilles.distribution(analysis_ids = 103)
age_dist$per_site[["server_a"]]

# Multiple distribution analyses
dists <- ds.omop.achilles.distribution(c(103, 105), scope = "per_site")
} # }
```
