# Recursively coerce named atomic vectors to lists

[`jsonlite::toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
drops the names of a named atomic vector (it emits a bare array), but
keeps the names of a list (as object keys). Walking the structure and
converting every named atomic vector to a list makes names survive the
JSON round-trip at any nesting depth. Unnamed atomic vectors are left
untouched so they stay JSON arrays, and data frames are left to
`toJSON`'s native row-wise encoding.

## Usage

``` r
.ds_coerce_names(x)
```

## Arguments

- x:

  An R object (list, vector, or scalar).

## Value

`x` with every named atomic vector turned into a named list.
