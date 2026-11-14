# Build an R call string with deparse-based argument formatting

Like `.build_code()` but escapes strings safely via `.codegen_value()`
and supports verbatim arguments created with `.codegen_raw()`. `NULL`
arguments are dropped.

## Usage

``` r
.codegen_call(fn_name, ...)
```

## Arguments

  - fn\_name:

    Character; fully qualified function name.

  - ...:

    Named arguments to include in the call.

## Value

Character string of the R call.
