# Force-encode a scalar string for the DataSHIELD expression transport

[`.ds_encode`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-ds_encode.md)
only base64-wraps lists and multi-element vectors; a scalar string
passes through as a bare literal. That is a problem for the filter
operator: the comparison operators (`">="`, `">"`, `"=="`, ...) contain
`<`, `>`, `=` characters that the DataSHIELD expression lexer (DSLite
and Opal) refuses inside a bare string literal ("Syntax error").
Wrapping the value as a single-element list routes it through the same
URL-safe base64 path, and the server-side `.ds_arg` transparently
decodes it back to the scalar.

## Usage

``` r
.ds_encode_scalar(x)
```

## Arguments

- x:

  A length-1 character value.

## Value

A `B64:`-prefixed token that the server's `.ds_arg` decodes back to the
scalar string.
