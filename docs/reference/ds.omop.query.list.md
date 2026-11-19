# List available query templates (DEPRECATED)

Deprecated shim for `ds.omop.analysis.list`. The curated query templates
are now part of the unified analysis catalog; this forwards to
`ds.omop.analysis.list()` and returns the catalog data frame.

## Usage

``` r
ds.omop.query.list(
  domain = NULL,
  provider = "native",
  symbol = "omop",
  conns = NULL,
  datasources = NULL
)
```

## Arguments

  - domain:
    
    Character; optional domain filter. `NULL` (the default) returns all
    domains.

  - provider:
    
    Character; ignored (retained for back-compatibility).

  - symbol:
    
    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

Data frame of analysis-catalog entry metadata (the pooled view).

## See also

`ds.omop.analysis.list`

## Examples

``` r
if (FALSE) { # \dontrun{
templates <- ds.omop.query.list()
head(templates)
} # }
```
