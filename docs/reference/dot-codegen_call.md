# Build an R call string with deparse-based argument formatting

Like
[`.build_code()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-build_code.md)
but escapes strings safely via
[`.codegen_value()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-codegen_value.md)
and supports verbatim arguments created with
[`.codegen_raw()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-codegen_raw.md).
`NULL` arguments are dropped.

## Usage

``` r
.codegen_call(fn_name, ...)
```

## Arguments

- fn_name:

  Character; fully qualified function name.

- ...:

  Named arguments to include in the call.

## Value

Character string of the R call.
