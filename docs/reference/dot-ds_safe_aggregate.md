# Resilient datashield.aggregate that tolerates per-server failures

Calls each server individually and returns partial results when some
servers fail (e.g., table not present on one server). Failed servers —
including a missing or NULL DSI response — are omitted from the result
and their errors are attached as an attribute.

## Usage

``` r
.ds_safe_aggregate(conns, expr)
```

## Arguments

- conns:

  DSI connections object.

- expr:

  The call expression to evaluate.

## Value

Named list of results (only successful servers).
