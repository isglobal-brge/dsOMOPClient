# Format an R value for recipe code generation

Deparse-based replacement for `.format_r_value()`: `deparse()` escapes
quotes/backslashes in strings correctly, keeps the `L` suffix on
integers, and emits valid literals for (nested) named lists.

## Usage

``` r
.codegen_value(x)
```

## Arguments

  - x:

    An R value, or a string wrapped with `.codegen_raw()`.

## Value

Character string of valid R code.
