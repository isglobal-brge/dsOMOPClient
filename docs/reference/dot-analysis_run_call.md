# Build the (possibly scope-bearing) server-side analysis run call

Constructs the unevaluated DataSHIELD call for `omopAnalysisRunDS` /
`omopAnalysisRunAssignDS`. `params` is JSON/base64-encoded for Opal
transport
([`.ds_encode`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-ds_encode.md));
`scope_args` is spliced as closed, named arguments. Table scopes are
bare symbols, never nested calls. `combine` is passed by name so absent
scope arguments cannot shift it into the wrong positional slot.

## Usage

``` r
.analysis_run_call(
  fn,
  res_symbol,
  name,
  params,
  scope_args,
  combine,
  date_handling = NULL
)
```

## Arguments

- fn:

  Character; the server method name.

- res_symbol:

  Character; the server-side handle symbol.

- name:

  Character; the catalog entry name.

- params:

  Named list of parameter values.

- scope_args:

  `NULL` or the closed named argument list returned by
  [`.analysis_scope_expr`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-analysis_scope_expr.md).

- combine:

  Character; `"union"` or `"intersect"`.

## Value

An unevaluated `call`.
