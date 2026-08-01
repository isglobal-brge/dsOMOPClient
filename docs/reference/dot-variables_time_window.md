# Derive an index-relative temporal spec from variables' time windows

Variable `time_window`s are index-relative day offsets
(`list(start=, end=)`). One output stream may carry one common window;
differing scopes must be split by the caller/compiler and are rejected
here rather than unioned (which would broaden at least one variable).

## Usage

``` r
.variables_time_window(vars)
```

## Arguments

- vars:

  List of `omop_variable` objects.

## Value

A temporal spec list with `index_window`, or `NULL`.
