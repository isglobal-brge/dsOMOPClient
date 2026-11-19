# Create a safe numeric value filter using server-computed bins

Convenience wrapper that first calls `ds.omop.safe.cutpoints` to obtain
disclosure-safe bin edges, then creates an `omop_filter_value` filter
whose boundary is snapped to the nearest safe bin edge. This ensures
that any subsequent filtering operation will not inadvertently create
small cells that violate disclosure controls.

## Usage

``` r
ds.omop.safe.filter.value(
  table,
  column,
  threshold,
  direction = c("above", "below"),
  concept_id = NULL,
  n_bins = 10L,
  symbol = "omop",
  conns = NULL,
  datasources = NULL
)
```

## Arguments

  - table:
    
    Character; the OMOP CDM table name (e.g., `"measurement"`).

  - column:
    
    Character; the numeric column to filter on (e.g.,
    `"value_as_number"`).

  - threshold:
    
    Numeric; the desired threshold value. The function snaps this to the
    nearest safe bin boundary.

  - direction:
    
    Character; `"above"` (default) or `"below"`, indicating whether to
    keep values above or below the threshold.

  - concept\_id:
    
    Integer or NULL; optional concept ID to restrict rows before
    computing cutpoints (default: NULL).

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

An `omop_filter` object of type `value_bin`, suitable for passing to
query or extraction functions.

## Examples

``` r
if (FALSE) { # \dontrun{
filt <- ds.omop.safe.filter.value("measurement", "value_as_number",
                                   threshold = 100, direction = "above",
                                   concept_id = 3004249)
} # }
```
