# Get Achilles count results

Queries the `achilles_results` table on each connected server for the
specified analysis IDs. These are pre-computed count-based statistics
(e.g., person counts by gender, condition frequency). By default (as
`ds.table` does) the result has both views: counts are summed across
servers with suppression propagation, and a stratum suppressed or absent
on any server is left out of the pooled table.

## Usage

``` r
ds.omop.achilles.results(
  analysis_ids,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - analysis\_ids:
    
    Integer vector; the Achilles analysis IDs to retrieve (e.g.,
    `c(1, 2, 400)` for total persons, gender breakdown, and condition
    frequency).

  - scope:
    
    Deprecated alias of `type` (`"per_site"` or `"pooled"`).

  - pooling\_policy:
    
    Character; `"strict"` (the default) leaves out a stratum absent on
    any server; `"pooled_only_ok"` sums the strata the servers returned.
    A stratum suppressed (NA) on any server is left out under both
    policies. See
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
    
    Result view: `"both"` (default), `"split"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result` object. The `per_site` element contains per-server
data frames with columns `analysis_id`, `stratum_1` through `stratum_5`,
and `count_value`. The `pooled` element (if requested) contains the
aggregated data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
# Total person count (analysis 1) per server and pooled
res <- ds.omop.achilles.results(analysis_ids = 1)
res$pooled

# Gender breakdown per site only
gender <- ds.omop.achilles.results(analysis_ids = 2, type = "split")
gender$per_site
} # }
```
