# Get Achilles Heel data-quality warnings (per site)

Achilles Heel counts records and does not provide distinct-person
support for each fired rule. It therefore cannot satisfy dsOMOP's
person-level contribution contract and is no longer available through
DataSHIELD. Run it only in a controller-side quality-assurance workflow.
This function always stops with that explanation, without contacting any
server; it is kept so existing code fails with a clear reason.

## Usage

``` r
ds.omop.achilles.heel(symbol = "omop", conns = NULL, datasources = NULL)
```

## Arguments

  - symbol:
    
    Character; session symbol (default "omop"). Unused.

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

Nothing: the function always stops with an error.
