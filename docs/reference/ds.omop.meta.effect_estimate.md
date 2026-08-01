# Meta-analyze a comparative effect estimate across databases (evidence synthesis)

The CLIENT half of OHDSI evidence synthesis: run a per-site fitted
comparative effect estimate on every server, then INVERSE-VARIANCE
meta-analyze the per-site log-estimates into ONE pooled estimate + 95%
CI (the
[`metafor::rma`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
pattern by hand — no new dependency). A single site cannot compute a
cross-database pooled estimate, so the server-side
`dsomop:cm.effect_estimate` (CohortMethod HR/RR; the `es_cm_result`
delegate) and `dsomop:sccs.incidence_rate_ratio` (SCCS IRR; the
`es_sccs_result` delegate) each emit only the disclosure-safe per-site
`log_estimate` + SE; this function pools them.

## Usage

``` r
ds.omop.meta.effect_estimate(
  name = "dsomop:cm.effect_estimate",
  params = list(),
  cohort = NULL,
  tables = NULL,
  combine = "union",
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- name:

  Character; the per-site effect-estimate analysis id. Default
  `"dsomop:cm.effect_estimate"` (CohortMethod). Use
  `"dsomop:sccs.incidence_rate_ratio"` for SCCS, or the `es_cm_result` /
  `es_sccs_result` evidence-synthesis ids.

- params:

  Named list of analysis params (e.g. `outcome_concept_id`,
  `model_type`); passed through to the per-site analysis unchanged.

- cohort:

  For CohortMethod, the two-population target+comparator scope (a
  length-2 set of cohort handles / ids / table names); for SCCS, the
  scoped case population.

- tables:

  Optional `omop.table` symbol scope (see
  [`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)).

- combine:

  Character; `"union"` (default) or `"intersect"` for multi-source scope
  folding.

- pooling_policy:

  Character; `"strict"` (default) or `"pooled_only_ok"`.

- symbol:

  Character; the session symbol (default `"omop"`).

- conns:

  DSI connection object(s) or `NULL` to use the session default.

## Value

A `dsomop_result`: `per_site` holds each server's gated per-site
effect-estimate frame; `pooled` holds the one-row meta-analysis (pooled
HR/RR + CI under both models, `n_databases`, `i2`, `tau2`).

## Details

Both a FIXED-effect and a random-effects (DerSimonian-Laird) pooled
estimate are returned, with Cochran's Q, \\I^2\\, and \\\tau^2\\
heterogeneity. No patient data crosses sites — only the already-gated
per-site sufficient statistics. A site whose per-site estimate the
server suppressed (small/empty arm) is ABSENT from the pool: under
`pooling_policy = "strict"` (default) any suppressed site aborts the
pool fail-closed; `"pooled_only_ok"` pools the remaining sites and
warns.

## See also

[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Pool a CohortMethod hazard ratio across databases.
res <- ds.omop.meta.effect_estimate(
  params = list(outcome_concept_id = 4329847),
  cohort = c(target_cohort, comparator_cohort))
res$pooled   # estimate_random, ci_lo_random, ci_hi_random, i2, ...
} # }
```
