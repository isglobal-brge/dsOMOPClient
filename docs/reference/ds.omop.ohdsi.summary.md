# Get OHDSI tool summary

Returns a summary of available result tables for a specific OHDSI tool.

## Usage

``` r
ds.omop.ohdsi.summary(tool_id, symbol = "omop", conns = NULL, type = NULL)
```

## Arguments

  - tool\_id:

    Character; which tool to summarize (e.g., `"cohort_diagnostics"`,
    `"cohort_incidence"`, `"characterization"`).

  - symbol:

    Character; the session symbol (default: `"omop"`).

  - conns:

    DSI connection object(s). If `NULL`, uses active session.

  - type:

    Optional result view: `"split"`, `"combine"`, or `"both"`. Tool
    summaries are site-specific, so a combined view has no value and
    records that reason.

## Value

A `dsomop_result` object with `scope = "per_site"`.

## Examples

``` r
if (FALSE) { # \dontrun{
cd_summary <- ds.omop.ohdsi.summary("cohort_diagnostics")
cd_summary$per_site
} # }
```
