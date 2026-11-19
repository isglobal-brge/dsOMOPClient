# Get a full schema snapshot

Retrieves a comprehensive schema snapshot from each connected server,
combining capabilities metadata (available tables, CDM version info)
with the join relationship graph into a single structure. This provides
a complete picture of the database schema that can be cached client-side
and used to drive headless schema exploration and query building.

## Usage

``` r
ds.omop.snapshot(symbol = "omop", conns = NULL, datasources = NULL)
```

## Arguments

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A named list (one element per server), where each element is a list with
`tables` (character vector of table names), `cdm_info` (list with CDM
version and DBMS details), and `edges` (data frame of join
relationships).

## Examples

``` r
if (FALSE) { # \dontrun{
snap <- ds.omop.snapshot()
snap$server1$tables
snap$server1$cdm_info
snap$server1$edges
} # }
```
