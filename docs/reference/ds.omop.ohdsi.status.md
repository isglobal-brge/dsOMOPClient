# Check OHDSI result tool availability

Queries each connected server to determine which OHDSI tool result
tables (CohortDiagnostics, CohortIncidence, Characterization, and
others) are present.

## Usage

``` r
ds.omop.ohdsi.status(
  symbol = "omop",
  conns = NULL,
  type = "split",
  datasources = NULL
)
```

## Arguments

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"split"` (default) or `"both"`. Status is
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
status <- ds.omop.ohdsi.status()
status$per_site[["server_a"]]$cohort_diagnostics$available
} # }
```
