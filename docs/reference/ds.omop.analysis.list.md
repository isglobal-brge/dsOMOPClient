# List unified analysis catalog entries

Returns metadata for every entry in the server's unified analysis
catalog — the single registry that folds the curated QueryLibrary SQL
templates, the pre-computed Achilles analyses, and the generic OHDSI
result tables behind one stable, pack-prefixed naming scheme
(`"dsomop:<id>"`). Because the catalog is defined by the server package,
the client requires an identical response from every participating
server before exposing a pooled view. No SQL, compute functions, or
other server internals are exposed.

## Usage

``` r
ds.omop.analysis.list(domain = NULL, symbol = "omop", conns = NULL)
```

## Arguments

- domain:

  Character; optional clinical-domain filter (e.g. `"condition"`,
  `"person"`). `NULL` (the default) returns entries for all domains.

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

A `dsomop_result` object with `scope = "pooled"`. The pooled element is
a data frame with one row per entry: `name` (the id), `domain`,
`adapter`, `mode`, disclosure `unit`, `description` (title), parameter
summary, the `accepts_cohort`/`accepts_tables` scoping flags, whether
the entry `requires_cohort` (un-scoped runs error), and whether it ships
a plot (`has_plot`).

## See also

[`ds.omop.analysis.get`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.get.md),
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)

## Examples

``` r
if (FALSE) { # \dontrun{
catalog <- ds.omop.analysis.list()
head(catalog$pooled)

# Only condition-domain analyses
cond <- ds.omop.analysis.list(domain = "condition")
cond$pooled$name
} # }
```
