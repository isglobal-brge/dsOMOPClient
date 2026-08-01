# Get OHDSI tool summary

Returns a summary of available result tables for a specific OHDSI tool.

## Usage

``` r
ds.omop.ohdsi.summary(tool_id, symbol = "omop", conns = NULL)
```

## Arguments

- tool_id:

  Character; which tool to summarize (e.g., `"cohort_diagnostics"`,
  `"cohort_incidence"`, `"characterization"`).

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL`, uses active session.

## Value

A `dsomop_result` object with `scope = "per_site"`.

## Examples

``` r
if (FALSE) { # \dontrun{
cd_summary <- ds.omop.ohdsi.summary("cohort_diagnostics")
cd_summary$per_site
} # }
```
