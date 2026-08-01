# Mark a string as already-generated R code

Wraps a code string so
[`.codegen_call()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-codegen_call.md)
injects it verbatim instead of deparsing it (used for nested constructor
calls such as filter lists).

## Usage

``` r
.codegen_raw(code)
```

## Arguments

- code:

  Character; R code to inject verbatim.

## Value

The string with class `"codegen_raw"`.
