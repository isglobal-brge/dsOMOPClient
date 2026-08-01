# List available query templates (DEPRECATED)

Deprecated shim for
[`ds.omop.analysis.list`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.list.md).
The curated query templates are now part of the unified analysis
catalog; this forwards to
[`ds.omop.analysis.list()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.list.md)
and returns the catalog data frame.

## Usage

``` r
ds.omop.query.list(
  domain = NULL,
  provider = "native",
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- domain:

  Character; optional domain filter. `NULL` (the default) returns all
  domains.

- provider:

  Character; ignored (retained for back-compatibility).

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

Data frame of analysis-catalog entry metadata (the pooled view).

## See also

[`ds.omop.analysis.list`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
templates <- ds.omop.query.list()
head(templates)
} # }
```
