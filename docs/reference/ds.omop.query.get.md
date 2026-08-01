# Get query template details (DEPRECATED)

Deprecated shim for
[`ds.omop.analysis.get`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.get.md).
Forwards to
[`ds.omop.analysis.get()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.get.md)
using the entry's pack-prefixed catalog name (`"dsomop:<query_id>"`) and
returns the entry metadata list.

## Usage

``` r
ds.omop.query.get(query_id, symbol = "omop", conns = NULL)
```

## Arguments

- query_id:

  Character; the legacy query ID (e.g., `"condition_prevalence"`).

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

Named list of catalog entry metadata, or `NULL` if not found.

## See also

[`ds.omop.analysis.get`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.get.md)

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- ds.omop.query.get("condition_prevalence")
meta$params
} # }
```
