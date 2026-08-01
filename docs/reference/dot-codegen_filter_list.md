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

A
[`.codegen_raw()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-codegen_raw.md)
string, or `NULL` when empty (so
[`.codegen_call()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-codegen_call.md)
drops the argument).
