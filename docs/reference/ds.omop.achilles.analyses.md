# List available Achilles analyses

Returns the catalog of Achilles analyses available on the connected
servers, optionally filtered by clinical domain. The catalog is defined
by the Achilles specification, so the pooled result is the catalog every
server returned identically; when the servers' catalogs differ, or a
server is unavailable, `$pooled` is `NULL` with `meta$pooling_reason`.

## Usage

``` r
ds.omop.achilles.analyses(
  domain = NULL,
  symbol = "omop",
  conns = NULL,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - domain:
    
    Character; optional domain filter. Valid values include `"person"`,
    `"condition"`, `"drug"`, `"measurement"`, `"procedure"`,
    `"observation"`, `"visit"`, and `"device"`. `NULL` (the default)
    returns all domains.

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
data frames listing analysis IDs, names, and associated domains.

## Examples

``` r
if (FALSE) { # \dontrun{
# All available analyses
catalog <- ds.omop.achilles.analyses()
catalog$pooled

# Only condition-related analyses
cond <- ds.omop.achilles.analyses(domain = "condition")
} # }
```
