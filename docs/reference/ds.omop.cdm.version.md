# Get the CDM version reported by each server

Returns the CDM version reported by each connected server (preferring
`cdm_source.cdm_version` and falling back to the version inferred from
the table structure). This is metadata, not patient data, so this reader
is not disclosure-gated. Sites may legitimately run different CDM
versions, so the result is deliberately kept **per-server** and is never
silently merged into a single version.

## Usage

``` r
ds.omop.cdm.version(
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
    
    Result view: `"split"` (default), `"both"`, or `"combine"`. The
    pooled view is the version record every server reported identically;
    when the servers differ it is `NULL` with `meta$pooling_reason`,
    never a silent merge.

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each server's result is a list with `cdm_version`,
`source`, and `vocabulary_version`.

## Examples

``` r
if (FALSE) { # \dontrun{
ver <- ds.omop.cdm.version()
# CDM version per server
lapply(ver$per_site, function(v) v$cdm_version)
} # }
```
