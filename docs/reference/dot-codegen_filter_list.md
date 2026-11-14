# Generate a verbatim \`filters = list(...)\` argument value

Generate a verbatim \`filters = list(...)\` argument value

## Usage

``` r
.codegen_filter_list(filters)
```

## Arguments

  - filters:

    List of `omop_filter`/`omop_filter_group` objects.

## Value

A `.codegen_raw()` string, or `NULL` when empty (so `.codegen_call()`
drops the argument).
