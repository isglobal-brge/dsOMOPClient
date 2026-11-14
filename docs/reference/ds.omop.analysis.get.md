# Get unified analysis catalog entry metadata

Returns full metadata for a single catalog entry: its parameter specs,
compute kind, disclosure spec, and scoping capabilities. Use it to
discover an entry's parameters and to check whether it accepts
cohort/table scoping before running it. Execution requires identical
metadata from every server; a mixed-version or partially unavailable
federation fails closed.

## Usage

``` r
ds.omop.analysis.get(name, symbol = "omop", conns = NULL, type = "both")
```

## Arguments

  - name:

    Character; the entry id (e.g. `"dsomop:achilles.401"`) or a
    shorthand for it (native id without the `"dsomop:"` prefix, or a
    unique id suffix; an ambiguous shorthand errors with the
    candidates).

  - symbol:

    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:

    DSI connection object(s). If `NULL` (the default), the connections
    stored in the active session are used.

  - type:

    Result view: `"both"` (default), `"split"`, or `"combine"`;
    dsBaseClient-style aliases are accepted. A split view can inspect
    nodes whose metadata differ; combined views require equality.

## Value

A `dsomop_result` object with `scope = "pooled"`. The pooled element is
a named list with the entry's `name`, `description`, `domain`, `mode`,
`params`, `compute_kind`, `disclosure`, `scope`, `adapter`, and the
inert client-side `plot` recipe (`NULL` when the entry ships none).
External packs also expose their pinned package/version and closed
output contract, so federated execution can require exact metadata
equality across nodes.

## See also

`ds.omop.analysis.list`, `ds.omop.analysis.run`

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- ds.omop.analysis.get("dsomop:achilles.401")
meta$pooled$params
meta$pooled$mode
} # }
```
