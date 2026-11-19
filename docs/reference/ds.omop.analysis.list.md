# List unified analysis catalog entries

Returns metadata for every entry in the server's unified analysis
catalog — the single registry that folds the curated QueryLibrary SQL
templates, the pre-computed Achilles analyses, and the generic OHDSI
result tables behind one stable, pack-prefixed naming scheme
(`"dsomop:<id>"`). Because the catalog is defined by the server package,
the pooled view is the catalog every participating server returned
identically. When the catalogs differ or a server is unavailable,
`$pooled` is `NULL` and `meta$pooling_reason` says why (`type =
"combine"` stops with that reason). No SQL, compute functions, or other
server internals are exposed.

## Usage

``` r
ds.omop.analysis.list(
  domain = NULL,
  symbol = "omop",
  conns = NULL,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - domain:
    
    Character; optional clinical-domain filter (e.g. `"condition"`,
    `"person"`). `NULL` (the default) returns entries for all domains.

  - symbol:
    
    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"both"` (default), `"split"`, or `"combine"`;
    dsBaseClient-style aliases are accepted. A pooled view requires
    equal catalogs. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result` object. The pooled element is a data frame with one
row per entry: `name` (the id), `domain`, `adapter`, `mode`, disclosure
`unit`, `description` (title), parameter summary, the
`accepts_cohort`/`accepts_tables` scoping flags, whether the entry
`requires_cohort` (un-scoped runs error), and whether it ships a plot
(`has_plot`).

## See also

`ds.omop.analysis.get`, `ds.omop.analysis.run`

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
