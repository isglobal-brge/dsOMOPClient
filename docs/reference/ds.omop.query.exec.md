# Execute a query template (DEPRECATED)

Deprecated shim for
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md).
Forwards to
[`ds.omop.analysis.run()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)
using the entry's pack-prefixed catalog name. For back-compatibility,
`"aggregate"` mode returns a named list of per-server data frames. The
legacy caller-selected `"assign"` mode is rejected: whether an analysis
is an assign loader is server-owned catalog metadata and must not be
asserted by the client. Use
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)
directly for catalog-managed loaders. Disclosure controls and
cross-server pooling are handled by that path.

## Usage

``` r
ds.omop.query.exec(
  query_id,
  inputs = list(),
  mode = "aggregate",
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- query_id:

  Character; the legacy query ID (e.g., `"condition_prevalence"`).

- inputs:

  Named list; parameter values for the entry. Default: empty list.

- mode:

  Character; only `"aggregate"` is accepted. The deprecated `"assign"`
  value now fails closed; assign behavior is selected from trusted
  server catalog metadata by
  [`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md).

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

A named list of per-server disclosure-controlled data frames. Use
[`ds.omop.query.pool`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.query.pool.md)
to combine them, or prefer the pooled view returned by
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)
directly.

## See also

[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- ds.omop.query.exec("condition_prevalence",
  inputs = list(concept_id = 201826))
pooled <- ds.omop.query.pool(results, query_id = "condition_prevalence")
} # }
```
