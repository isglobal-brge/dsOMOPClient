# Derive the exact reserved symbol family for an output mapping

A base symbol can be reused across ordinary, sparse, temporal and
person-period executions. Clearing this finite reserved family prevents
a component from a previous representation being mistaken for the
current result while leaving unrelated prefix-sharing user objects
untouched.

## Usage

``` r
.plan_output_symbol_families(out)
```

## Arguments

- out:

  Named output-to-base-symbol mapping.

## Value

Named list of exact reserved symbols per requested output.
