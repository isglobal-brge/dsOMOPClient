# Normalize imported filter params back to their constructed form

JSON parsing (`simplifyVector = FALSE`) turns atomic vectors into
unnamed lists of scalars, and loses the integer/double distinction. This
restores atomic vectors and re-applies the canonical storage types used
by the `omop_filter_*` constructors so an export/import round-trip
compares [`identical()`](https://rdrr.io/r/base/identical.html). Named
lists (e.g. `window`, `value`) are kept intact.

## Usage

``` r
.recipe_restore_params(params)
```

## Arguments

- params:

  Named list of filter parameters.

## Value

The normalized parameter list.
