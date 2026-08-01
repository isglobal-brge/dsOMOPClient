# Encode a complex R object as JSON for DataSHIELD transport

When passing complex R objects (lists, named vectors) through
datashield.assign.expr() or datashield.aggregate(), Opal serializes them
via deparse(), which generates structure()/c() calls not in the
DataSHIELD whitelist. This helper wraps them as JSON strings. The
server-side .ds_arg() transparently deserializes them. In DSLite, call()
handles native R objects directly, so this function only encodes
lists/complex objects — scalars pass through.

## Usage

``` r
.ds_encode(x)
```

## Arguments

- x:

  An R object to encode.

## Value

A JSON string if x is a list, or x unchanged if scalar.
