# Get OHDSI tool summary

Returns a summary of available result tables for a specific OHDSI tool.

## Usage

``` r
ds.omop.ohdsi.summary(
  tool_id,
  symbol = "omop",
  conns = NULL,
  type = "split",
  datasources = NULL
)
```

## Arguments

  - tool\_id:
    
    Character; which tool to summarize (e.g., `"cohort_diagnostics"`,
    `"cohort_incidence"`, `"characterization"`).

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"split"` (default) or `"both"`. Tool summaries are
    site-specific: `"both"` returns `pooled = NULL` with
    `meta$pooling_reason`, and `"combine"` stops before contacting the
    servers. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result` object (per-server view).

## Examples

``` r
if (FALSE) { # \dontrun{
cd_summary <- ds.omop.ohdsi.summary("cohort_diagnostics")
cd_summary$per_site
} # }
```
