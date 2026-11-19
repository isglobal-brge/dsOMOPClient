# Get the join relationship graph

Retrieves the join relationship graph for the OMOP CDM schema from each
connected server. The graph describes standard OMOP relationships (e.g.,
via `person_id`, `visit_occurrence_id`, or concept foreign keys). It is
an introspection aid; current recipe/plan execution does not consume it
to invent arbitrary joins automatically.

## Usage

``` r
ds.omop.joins(symbol = "omop", conns = NULL, datasources = NULL)
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

A named list (one element per server) of data frames with edge metadata
such as `from_table`, `to_table`, `from_column`, and `to_column`.

## Examples

``` r
if (FALSE) { # \dontrun{
joins <- ds.omop.joins()
joins$server1
} # }
```
