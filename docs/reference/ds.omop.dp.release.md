# Request a sticky privacy release

Performs a complete-federation preflight, requests the same typed release
from every node, verifies the returned mechanism contract, and optionally
pools only the noisy sufficient statistics. A failure at any site stops the
call without publishing another site's value. Servers may already have
committed their first answer; retrying the identical request returns that
complete answer. The default v3 contract requires dsOMOP 2.7.1 or later.
For a staged upgrade only, the client option
`dsomop.dp.allow_legacy_servers` (default `FALSE`) can be set to
`TRUE` to permit legacy v2 servers, including mixed federations. Every
legacy server is named in a warning: no first-answer binding: an unrotated
data refresh can reveal whether a released statistic changed; see
isglobal-brge/dsOMOP#20. The first-answer semantics below apply to v3 sites.
Public request identity is server-owned and binds authenticated canonical
dataset/recipe lineage, the typed statistic and mechanism contract, public
`snapshot_id`, and privacy epoch. The first answer retains the private
bounded-statistic fingerprint in its noise context. Valid later requests
return the stored answer without comparing fingerprints or re-noising,
even after any size of unrotated source update. The analyst's
`population_id` compatibility label and server symbol alias do not
participate. Only custodian snapshot or epoch rotation restores freshness.
Answers can remain stale indefinitely, and different requests first answered
at different times need not represent one coherent source snapshot.
For multiple sites, pooling a non-count statistic additionally requires one
compatible public dsOMOP harmonization contract for age grids, date
semantics, calendar-day granularity, UTC handling, week start, and
operational caps. Per-site output and pooled distinct-person counts do not
depend on those value semantics and therefore do not require that unrelated
contract.
Every input must have been produced by an audited person-local server path
and carry its authenticated content-bound provenance capsule.

## Usage

``` r
ds.omop.dp.release(
  x,
  privacy,
  datasources = NULL,
  pool = TRUE,
  format = c("long", "wide", "vector", "raw"),
  type = NULL
)
```

## Arguments

- x:

  One bare DataSHIELD symbol containing a server-side `omop.table`.

- privacy:

  An `omop_privacy` specification. If it does not contain an explicit
  `population_id`, the bare symbol `x` is used as its public
  compatibility label. This label does not control sticky identity.

- datasources:

  Named DataSHIELD connection list. `NULL` uses
  [`DSI::datashield.connections_find()`](https://datashield.github.io/DSI/reference/datashield.connections_find.html).

- pool:

  Logical; pool the complete set of noisy site releases.

- format:

  Client-only pooled-result format: long data frame, one-row wide data
  frame, named vector, or raw list. Histogram releases support all four
  forms; other statistics retain their typed list. This argument never
  enters the server specification or sticky-release identity.

- type:

  Optional result view: `"split"`, `"combine"`, or `"both"`. When omitted,
  `pool = TRUE` means both views and `pool = FALSE` means split only.

## Value

A `dsomop_result`. The `meta$privacy` record reports the
  effective population label, a named public snapshot map, fixed per-site
  epsilon and parallel cross-site composition for this release. Federated
  nodes are modeled as separate populations, so the combined epsilon and
  delta are the maxima of their per-site values. Multi-site results also
  carry `meta$harmonization` when non-count values were pooled.
  `meta$privacy$per_site_contract` records each server's protocol and
  state contract, including for `type = "combine"`.
  `legacy_servers` names all v2 sites and `mixed_contracts` reports
  whether v2 and v3 were combined. Shared contract fields that differ across
  sites are `NULL`; consult `per_site_contract` for their values.
  Legacy warnings are also retained in `meta$warnings`.

## Details

Every first answer uses the fixed epsilon reported by its server and delta
zero. Public new identities reserve persistent storage; existing identities
remain readable at capacity. There is no lifetime privacy budget or privacy
call quota. Composition metadata describes only the participating sites in
the current federated release. First-answer replay closes the successful
within-identity temporal equality selection; no full temporal transcript
DP or timing, admission, or private-triggered rotation guarantee is claimed.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- omop_privacy("count")
ds.omop.dp.release("analysis_table", p)
} # }
```
