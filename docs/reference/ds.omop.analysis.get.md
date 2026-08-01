# Get unified analysis catalog entry metadata

Returns full metadata for a single catalog entry: its parameter specs,
compute kind, disclosure spec, and scoping capabilities. Use it to
discover an entry's parameters and to check whether it accepts
cohort/table scoping before running it. Execution requires identical
metadata from every server; a mixed-version or partially unavailable
federation fails closed.

## Usage

``` r
ds.omop.analysis.get(name, symbol = "omop", conns = NULL)
```

## Arguments

- name:

  Character; the entry id (e.g. `"dsomop:achilles.401"`) or a shorthand
  for it (native id without the `"dsomop:"` prefix, or a unique id
  suffix; an ambiguous shorthand errors with the candidates).

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

A `dsomop_result` object with `scope = "pooled"`. The pooled element is
a named list with the entry's `name`, `description`, `domain`, `mode`,
`params`, `compute_kind`, `disclosure`, `scope`, `adapter`, and the
inert client-side `plot` recipe (`NULL` when the entry ships none).
External packs also expose their pinned package/version and closed
output contract, so federated execution can require exact metadata
equality across nodes.

## See also

[`ds.omop.analysis.list`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.list.md),
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- ds.omop.analysis.get("dsomop:achilles.401")
meta$pooled$params
meta$pooled$mode
} # }
```
