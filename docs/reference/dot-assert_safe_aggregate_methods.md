# Verify that generic raw-container AggregateMethods are unavailable

A DataSHIELD aggregate method mapped directly to base `c` or `list` can
wrap a protected server object and return it without a reviewed
disclosure gate. Connection therefore fails before resource assignment
unless every server provides a verifiable aggregate-method inventory
free of both direct names and aliases to those functions.

## Usage

``` r
.assert_safe_aggregate_methods(conns)
```

## Arguments

- conns:

  Named DataSHIELD connections.

## Value

The verified method inventory, invisibly.
