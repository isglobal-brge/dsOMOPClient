# Get Achilles count results

Queries the `achilles_results` table on each connected server for the
specified analysis IDs. These are pre-computed count-based statistics
(e.g., person counts by gender, condition frequency). When
`scope = "pooled"`, counts are summed across servers with suppression
propagation: if any server suppressed a cell (NA), the pooled cell is
also set to NA to prevent disclosure.

## Usage

``` r
ds.omop.achilles.results(
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

  Integer vector; the Achilles analysis IDs to retrieve (e.g.,
  `c(1, 2, 400)` for total persons, gender breakdown, and condition
  frequency).

- scope:

  Character; `"per_site"` returns each server's results separately,
  `"pooled"` additionally aggregates counts across servers.

- pooling_policy:

  Character; controls how suppressed (NA) cells are handled during
  pooling. `"strict"` (the default) sets the pooled value to NA if any
  server suppressed the cell. `"pooled_only_ok"` sums only the
  non-suppressed values.

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

A `dsomop_result` object. The `per_site` element contains per-server
data frames with columns `analysis_id`, `stratum_1` through `stratum_5`,
and `count_value`. The `pooled` element (if requested) contains the
aggregated data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get total person count (analysis 1) pooled across servers
res <- ds.omop.achilles.results(analysis_ids = 1, scope = "pooled")
res$pooled

# Get gender breakdown per site
gender <- ds.omop.achilles.results(analysis_ids = 2)
gender$per_site
} # }
```
