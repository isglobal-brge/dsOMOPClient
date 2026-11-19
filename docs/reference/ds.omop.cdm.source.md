# Get the CDM source description from each server

Returns the full `cdm_source` table row(s) from each connected server,
describing the data source (name, abbreviation, holder, release/version
dates, etc.). This is metadata, not patient data, so this reader is not
disclosure-gated. Sites may describe genuinely different data sources,
so the result is deliberately kept **per-server** and is not merged.

## Usage

``` r
ds.omop.cdm.source(
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
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

  - execute:
    
    Logical; if `FALSE`, returns a dry-run `dsomop_result` containing
    only the reproducible R code without contacting the servers.

  - type:
    
    Result view: `"split"` (default) or `"both"`. Data source
    descriptions are site-specific: `"both"` returns `pooled = NULL`
    with `meta$pooling_reason`, and `"combine"` stops before contacting
    the servers.

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result` (per-server view). Each server's result is its
`cdm_source` data frame (empty if the table is absent). `pooled` is
`NULL` by design.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- ds.omop.cdm.source()
src$per_site            # one cdm_source frame per server
} # }
```
