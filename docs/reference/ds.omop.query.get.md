# Get query template details (DEPRECATED)

Deprecated shim for `ds.omop.analysis.get`. Forwards to
`ds.omop.analysis.get()` using the entry's pack-prefixed catalog name
(`"dsomop:<query_id>"`) and returns the entry metadata list.

## Usage

``` r
ds.omop.query.get(query_id, symbol = "omop", conns = NULL, datasources = NULL)
```

## Arguments

  - query\_id:
    
    Character; the legacy query ID (e.g., `"condition_prevalence"`).

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

Named list of catalog entry metadata, or `NULL` if not found.

## See also

`ds.omop.analysis.get`

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- ds.omop.query.get("condition_prevalence")
meta$params
} # }
```
