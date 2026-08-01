# Format an R value for recipe code generation

Deparse-based replacement for
[`.format_r_value()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-format_r_value.md):
[`deparse()`](https://rdrr.io/r/base/deparse.html) escapes
quotes/backslashes in strings correctly, keeps the `L` suffix on
integers, and emits valid literals for (nested) named lists.

## Usage

``` r
.codegen_value(x)
```

## Arguments

- x:

  An R value, or a string wrapped with
  [`.codegen_raw()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-codegen_raw.md).

## Value

Character string of valid R code.
