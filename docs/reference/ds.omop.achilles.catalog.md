# Get Achilles analysis catalog

Returns the full catalog of available Achilles analyses from the server.
If the `achilles_analysis` table exists, its contents are returned
directly; otherwise, the server discovers available analyses by
inspecting distinct `analysis_id` values in the `achilles_results`
table. The pooled result is the catalog every server returned
identically; when the servers' catalogs differ, or a server is
unavailable, `$pooled` is `NULL` with `meta$pooling_reason`.

## Usage

``` r
ds.omop.achilles.catalog(
  symbol = "omop",
  conns = NULL,
  type = "both",
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
    
    Result view: `"both"` (default), `"split"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry and the pooled consensus are
data frames listing the available analysis IDs and their descriptions.

## Examples

``` r
if (FALSE) { # \dontrun{
catalog <- ds.omop.achilles.catalog()
head(catalog$pooled)
} # }
```
