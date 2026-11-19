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
  execute = TRUE,
  type = "split",
  datasources = NULL
)
```

## Arguments

  - analysis\_ids:
    
    Integer vector; distribution analysis IDs to retrieve (e.g.,
    `c(103, 105)` for age and observation length distributions).

  - scope:
    
    Deprecated alias of `type` (`"per_site"` or `"pooled"`).

  - pooling\_policy:
    
    Character; `"strict"` (the default) or `"pooled_only_ok"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - symbol:
    
    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - execute:
    
    Logical; if `FALSE`, returns a dry-run `dsomop_result` containing
    only the reproducible R code without contacting the servers.

  - type:
    
    Result view: `"split"` (default), `"both"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result` object. Per-site results contain distribution data
frames with columns `analysis_id`, `stratum_1`, `count_value`,
`avg_value`, `stdev_value`, `p10_value`, `p25_value`, `median_value`,
`p75_value`, `p90_value`.

## Details

Like `ds.mean`, the default view is per server. A pooled view (`type =
"both"`) combines the counts, count-weighted averages and pooled SDs;
percentile values cannot be combined from pre-computed summaries, so all
percentile columns in the pooled result are `NA`. Use per-site data for
percentile display.

## Examples

``` r
if (FALSE) { # \dontrun{
# Age distribution (analysis 103) per site
age_dist <- ds.omop.achilles.distribution(analysis_ids = 103)
age_dist$per_site[["server_a"]]

# Multiple distribution analyses, pooled as well
dists <- ds.omop.achilles.distribution(c(103, 105), type = "both")
} # }
```
