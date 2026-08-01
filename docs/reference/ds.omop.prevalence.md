# Covariate prevalence over a cohort, in one call

Thin wrapper over
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)
for the catalog's feature-prevalence analysis
(`"dsomop:fe.prevalence"`): the per-covariate distinct-person count and
proportion over a scoped cohort, for one clinical domain. It builds the
analysis params and delegates, so cohort/table scoping, cross-server
pooling, optional plotting, and the ONE per-patient disclosure gate are
inherited unchanged.

## Usage

``` r
ds.omop.prevalence(
  concept_id = NULL,
  cohort = NULL,
  domain = "condition",
  top_n = 50,
  tables = NULL,
  plot = FALSE,
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- concept_id:

  Integer vector or `NULL`; when supplied, the gated result is narrowed
  to these covariate concept id(s) (a post-gate row subset). `NULL` (the
  default) returns the domain's top covariates.

- cohort:

  Cohort reference to scope to: a `dsomop_cohort_handle`, a
  `cohort_definition_id`, or a server-side cohort table name. Required
  unless `tables` is given.

- domain:

  Character; clinical domain by name (`"condition"`, `"drug"`,
  `"procedure"`, `"measurement"`, `"observation"`) or its code
  (`"0"`-`"4"`). Default `"condition"`.

- top_n:

  Integer; number of top covariates to return (default 50).

- tables:

  Optional character vector of `omop.table` symbol names to scope to
  (their distinct persons); may be combined with `cohort`.

- plot:

  Logical; build the entry's client-side plot over the gated data
  (default `FALSE`). See
  [`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md).

- symbol:

  Character; the session symbol (default `"omop"`).

- conns:

  DSI connection object(s) or `NULL` to use the session default.

## Value

A `dsomop_result` (see
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)).

## Details

Because the cohort IS the analysis population, a `cohort` (or `tables`)
scope is required; an un-scoped call fails closed with a clear error
from the server rather than returning an empty frame.

## See also

[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md),
[`ds.omop.distribution`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.distribution.md),
[`ds.omop.cohort.create`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.create.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Simplest path: top condition prevalence over a cohort, one call.
ds.omop.prevalence(cohort = my_cohort)

# A specific concept's prevalence (drug domain).
ds.omop.prevalence(concept_id = 1503297, cohort = my_cohort, domain = "drug")
} # }
```
