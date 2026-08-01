# Reject variables whose value_source / raw column is a blocked column

Reject variables whose value_source / raw column is a blocked column

## Usage

``` r
.assert_no_blocked_value_sources(variables)
```

## Arguments

- variables:

  Named list of `omop_variable` objects.

## Value

Invisibly TRUE, or stops with a disclosure error.
