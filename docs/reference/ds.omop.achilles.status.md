# Check Achilles availability

Queries each connected server to determine whether Achilles result
tables (`achilles_results` and `achilles_results_dist`) are present and
accessible. Returns per-site availability status and table row counts.
No pooling is performed because this is metadata-only.

## Usage

``` r
ds.omop.achilles.status(
  symbol = "omop",
  conns = NULL,
  type = "split",
  datasources = NULL
)
```

## Arguments

  - symbol:
    
    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

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

A `dsomop_result` object (per-server view). Each server's result is a
list or data frame indicating whether the Achilles tables exist and how
many rows they contain.

## Examples

``` r
if (FALSE) { # \dontrun{
status <- ds.omop.achilles.status()
status$per_site[["server_a"]]$available
} # }
```
