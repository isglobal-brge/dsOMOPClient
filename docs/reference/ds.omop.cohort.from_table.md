# Build a cohort from the persons in a server-side omop.table symbol

Turns an existing server-side, token-keyed `omop.table` symbol – e.g.
the symbol produced by
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)
or one of the data-manipulation verbs
([`ds.omop.merge`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.merge.md)
etc.) – into a reusable cohort that can scope subsequent exploration
queries and plan/recipe runs. The CLIENT sends only the symbol NAME; the
server reads its distinct person tokens, reverses them to original ids
with the per-resource key, gates the distinct count (fail-closed), and
materialises a size-checked cohort temp table. No identifier ever leaves
the server.

## Usage

``` r
ds.omop.cohort.from_table(x, new_name = NULL, symbol = "omop", conns = NULL)
```

## Arguments

- x:

  Character; the name of a server-side `omop.table` symbol.

- new_name:

  Character; TABLE name for the cohort. If `NULL` (the default), an
  auto-generated name is used.

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

Invisibly; a `dsomop_cohort_handle` carrying the server-side TABLE name.
Pass it straight into the `cohort` argument of the exploration wrappers
(e.g. `ds.omop.concept.prevalence`), into
[`ds.omop.cohort.combine()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.combine.md),
or as a plan/recipe population scope.

## Disclosure control

The derived cohort is gated on its distinct-subject count: if the source
symbol resolves to fewer than the server's per-subset threshold
(`nfilter_subset`) persons, the call FAILS CLOSED with an "insufficient
individuals" error and no cohort table is materialised. The error
reflects the contents of the symbol you supplied and carries no
disclosure about any pre-existing cohort.

## See also

[`ds.omop.cohort.create`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.create.md),
[`ds.omop.cohort.combine`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.combine.md)

## Examples

``` r
if (FALSE) { # \dontrun{
feats <- ds.omop.plan.execute(plan, out = c(features = "F"))
coh <- ds.omop.cohort.from_table("F")
ds.omop.concept.prevalence("condition_occurrence", cohort = coh)
} # }
```
