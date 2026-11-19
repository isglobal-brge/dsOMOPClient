# List discovered OHDSI result tables

Returns a catalog of all OHDSI result tables found across connected
servers, including tool identification and row counts.

## Usage

``` r
ds.omop.ohdsi.tables(
  symbol = "omop",
  conns = NULL,
  type = "both",
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
    
    Result view: `"both"` (default), `"split"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. `$pooled` stacks the servers' catalogs with a
`.server` column; it is `NULL` (with `meta$pooling_reason`) when a
server is unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
tables <- ds.omop.ohdsi.tables()
tables$pooled
} # }
```
