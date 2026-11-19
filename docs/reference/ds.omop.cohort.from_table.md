# Build a cohort from the persons in a server-side omop.table symbol

Turns an existing server-side, token-keyed `omop.table` symbol – e.g.
the symbol produced by `ds.omop.plan.execute` or one of the
data-manipulation verbs (`ds.omop.merge` etc.) – into a reusable cohort
that can scope subsequent exploration queries and plan/recipe runs. The
CLIENT sends only the symbol NAME; the server reads its distinct person
tokens, reverses them to original ids with the per-resource key, gates
the distinct count (fail-closed), and materialises a size-checked cohort
temp table. No identifier ever leaves the server.

## Usage

``` r
ds.omop.cohort.from_table(
  x,
  new_name = NULL,
  symbol = "omop",
  conns = NULL,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

  - x:
    
    Character; the name of a server-side `omop.table` symbol.

  - new\_name:
    
    Deprecated alias of `newobj`.

  - symbol:
    
    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - newobj:
    
    Character; TABLE name for the cohort (of the form
    `dsomop_cohort_fromtbl_<suffix>`), as dsBaseClient assigning
    functions call it. If `NULL` (the default), an auto-generated name
    is used.

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

Invisibly; a `dsomop_cohort_handle` carrying the server-side TABLE name.
Pass it straight into the `cohort` argument of the exploration wrappers
(e.g. `ds.omop.concept.prevalence`), into `ds.omop.cohort.combine()`, or
as a plan/recipe population scope.

## Disclosure control

The derived cohort is gated on its distinct-subject count: if the source
symbol resolves to fewer than the server's per-subset threshold
(`nfilter_subset`) persons, the call FAILS CLOSED with an "insufficient
individuals" error and no cohort table is materialised. The error
reflects the contents of the symbol you supplied and carries no
disclosure about any pre-existing cohort.

## See also

`ds.omop.cohort.create`, `ds.omop.cohort.combine`

## Examples

``` r
if (FALSE) { # \dontrun{
feats <- ds.omop.plan.execute(plan, out = c(features = "F"))
coh <- ds.omop.cohort.from_table("F")
ds.omop.concept.prevalence("condition_occurrence", cohort = coh)
} # }
```
