# Create a safe population filter for a numeric measurement interval

Requests the controller-configured public grid for one measurement
concept, then snaps the requested closed-open interval outwards to edges
issued by every connected site. The result is executable as a
population-level `has_measurement` filter. Exact or one-sided
client-authored thresholds are deliberately not supported.

## Usage

``` r
ds.omop.safe.filter.measurement(
  concept_id,
  min_value,
  max_value,
  n_bins = 10L,
  symbol = "omop",
  conns = NULL,
  datasources = NULL
)
```

## Arguments

  - concept\_id:
    
    One measurement concept ID.

  - min\_value, max\_value:
    
    Finite requested interval limits. Both are required and must lie
    inside the common public grid.

  - n\_bins:
    
    Integer; the number of bins for cutpoint computation (default: 10).

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

An authenticated population-level `omop_filter`.
