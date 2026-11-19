# Get a disclosure-safe numeric histogram

Computes a binned histogram for a numeric column in an OMOP CDM table.
For a pooled view (`type = "both"` or `"combine"`), a two-pass algorithm
is used: the first pass collects p05/p95 ranges from each server to
compute shared bin edges, and the second pass counts persons per bin
using those shared edges so that results are directly comparable and
summable across servers. Bins with counts below the disclosure threshold
are left out by the server; pooled bins are matched by their edges.

## Usage

``` r
ds.omop.value.histogram(
  table,
  value_col,
  bins = 20L,
  concept_id = NULL,
  cohort_table = NULL,
  window = NULL,
  cohort = NULL,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  plot = FALSE,
  nbins = 9L,
  xlab = NULL,
  main = NULL,
  col = "#4C72B0",
  type = "split",
  datasources = NULL
)
```

## Arguments

  - table:
    
    Character; the CDM table name (e.g., `"measurement"`).

  - value\_col:
    
    Character; the numeric column to histogram (e.g.,
    `"value_as_number"`).

  - bins:
    
    Integer; the number of histogram bins (default: 20).

  - concept\_id:
    
    Integer or NULL; optional concept ID to restrict rows to a single
    concept of the table before binning (e.g. `value_as_number` for one
    measurement concept). Default: NULL for all rows. The server applies
    the same disclosure controls to the concept-filtered population.
    Requires a dsOMOP server build with histogram concept scoping; older
    servers reject the argument (use the concept-scoped quantiles
    aggregate via `ds.omop.value.quantiles()` as a fallback).

  - cohort\_table:
    
    Character; name of a server-side cohort temp table for filtering, or
    NULL (default: NULL).

  - window:
    
    List with `start`/`end` date strings for temporal filtering, or NULL
    (default: NULL).

  - cohort:
    
    Cohort reference (a `dsomop_cohort_handle`, a
    `cohort_definition_id`, or a server-side cohort table name), or
    NULL. Takes precedence over `cohort_table`.

  - scope:
    
    Deprecated alias of `type` (`"per_site"` or `"pooled"`).

  - pooling\_policy:
    
    Character; `"strict"` (default) or `"pooled_only_ok"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - execute:
    
    Logical; if `FALSE`, return a dry-run result containing only the
    generated call code (default: `TRUE`).

  - plot:
    
    Logical; if `TRUE`, draw a federation-wide bar chart of the pooled,
    shared-edge bins (the default split view becomes `"both"`) and
    return the result invisibly. Default `FALSE`.

  - nbins:
    
    Integer; number of display bins used when `plot = TRUE` (default:
    9).

  - xlab, main, col:
    
    Axis label, title and bar colour used when `plot = TRUE`.

  - type:
    
    Result view: `"split"` (default), `"both"`, or `"combine"`. A pooled
    plot requires `"combine"` or `"both"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry is a data frame with
`bin_start`, `bin_end` and `count` (banded persons; bins below the
threshold are left out). `$pooled` sums `count` per bin on the shared
edges, matching bins by `bin_start` and `bin_end`; a bin missing on a
server is not treated as zero. When the shared range cannot be formed,
the per-server histograms (on each server's own edges) are still
returned and `meta$pooling_reason` says why nothing was pooled.

## Examples

``` r
if (FALSE) { # \dontrun{
hist_result <- ds.omop.value.histogram("measurement", "value_as_number",
                                        bins = 30, scope = "pooled")
hist_result$pooled
} # }
```
