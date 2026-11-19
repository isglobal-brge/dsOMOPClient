# Disconnect an OMOP session

Closes each server-side database handle, removes its temporary/staged
artifacts and then removes the public OMOP symbol. Every participating
node must report success and symbol removal is verified. On failure the
local session registry is retained so cleanup can be retried.

## Usage

``` r
ds.omop.disconnect(symbol = "omop", conns = NULL, datasources = NULL)
```

## Arguments

  - symbol:
    
    Character; the session symbol to disconnect (default: "omop").

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

Invisible TRUE on success.

## See also

`ds.omop.connect`

## Examples

``` r
if (FALSE) { # \dontrun{
ds.omop.disconnect("omop")
} # }
```
