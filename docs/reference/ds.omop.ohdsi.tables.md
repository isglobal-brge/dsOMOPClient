# List discovered OHDSI result tables

Returns a catalog of all OHDSI result tables found across connected
servers, including tool identification and row counts.

## Usage

``` r
ds.omop.ohdsi.tables(symbol = "omop", conns = NULL)
```

## Arguments

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL`, uses active session.

## Value

A `dsomop_result` object. Pooled is the union of catalogs.

## Examples

``` r
if (FALSE) { # \dontrun{
tables <- ds.omop.ohdsi.tables()
tables$pooled
} # }
```
