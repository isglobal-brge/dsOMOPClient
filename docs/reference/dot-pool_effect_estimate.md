# Inverse-variance meta-analysis of per-site log-effect estimates

The cross-database EVIDENCE SYNTHESIS half that the server cannot do
single-site: pool each site's per-site effect estimate (a log
hazard/rate ratio from `dsomop:cm.effect_estimate`, or a log IRR from
`dsomop:sccs.incidence_rate_ratio`) into ONE pooled estimate + 95% CI by
inverse-variance weighting — the
[`metafor::rma`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
pattern done by hand (no new dependency). Both a FIXED-effect and a
random-effects (DerSimonian-Laird) pool are returned, plus Cochran's Q,
\\I^2\\, and \\\tau^2\\.

## Usage

``` r
.pool_effect_estimate(per_site_logest, per_site_se, policy = "strict")
```

## Arguments

- per_site_logest:

  Named numeric vector of per-site log-effect estimates.

- per_site_se:

  Named numeric vector of per-site SEs of the log-estimate (same names).

- policy:

  Character; `"strict"` (any NA site aborts) or `"pooled_only_ok"` (drop
  NA sites, pool the rest).

## Value

List with `$result` (a one-row data.frame: pooled HR/RR + CI under both
models, plus `n_databases`, `q`, `i2`, `tau2`) and `$warnings`.

## Details

No patient data crosses sites: the inputs are the already
disclosure-safe per-site `log_estimate` + `se_log_estimate` (the
inverse-variance sufficient statistics). A site whose per-site estimate
was SUPPRESSED (NA / small arm, the server fail-closed) is simply ABSENT
from the pool — exactly the `pooled_only_ok` behaviour of
[`.pool_variance`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-pool_variance.md);
under `strict` any NA site aborts the pool fail-closed.
