# Request a sticky privacy release

Performs a complete-federation preflight, requests the same typed
release from every node, verifies the returned mechanism contract, and
optionally pools only the noisy sufficient statistics. A failure at any
site stops the call without publishing another site's value. Retrying
the identical request returns the same complete answer when its bounded
contributions and applicable fixed-synopsis generation are unchanged.

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
    `DSI::datashield.connections_find()`.

  - pool:

    Logical; pool the complete set of noisy site releases.

  - format:

    Client-only pooled-result format: long data frame, one-row wide data
    frame, named vector, or raw list. Histogram releases support all
    four forms; other statistics retain their typed list. This argument
    never enters the server specification or sticky-release identity.

  - type:

    Optional result view: `"split"`, `"combine"`, or `"both"`. When
    omitted, the legacy `pool` argument is preserved: `TRUE` means both
    views and `FALSE` means split only.

## Value

A `dsomop_result`. On 2.10.3, `meta$privacy` reports each site's
protection route and guarantee scope. Only all-fixed federations report
vector epsilon composition (maximum for disjoint sites, sum for
overlapping sites), never an API-transcript epsilon. Heuristic and
legacy warnings remain in `meta$warnings`. For older servers,
`meta$privacy` reports the effective population label, a named public
snapshot map, fixed per-site epsilon and parallel cross-site composition
for this release. Federated nodes are modeled as separate populations,
so the combined epsilon and delta are the maxima of their per-site
values. Multi-site results also carry `meta$harmonization` when
non-count values were pooled. `meta$privacy$per_site_contract` records
each server's protocol and state contract, including for `type =
"combine"`. `legacy_servers` names all v2 sites and `mixed_contracts`
reports whether servers with different DP contracts were combined (v2
and v3, or the seed-only and first-answer bindings). Shared contract
fields that differ across sites are `NULL`; consult `per_site_contract`
for their values. Legacy warnings are also retained in `meta$warnings`.

## Details

On dsOMOP 2.10.3, typed binary rates carry numerator/denominator masks
of the complete registered domain; numeric histograms use one versioned
projection of their complete atom basis before coarsening. Pooling sums
compatible signed bases and derives the view once. Fixed synopses
declare their bounded-vector epsilon and delta zero before exact support
gates; realised-contribution releases are heuristic and declare neither.
No finite epsilon describes the whole service transcript. The public
`snapshot_id` is optional display metadata and never controls admission
or noise. All older servers receive an explicit upgrade warning. A mixed
old/new federation supports `type = "split"` only.

The remaining legacy semantics apply only to servers before 2.10.3.
Their default v3 contract requires dsOMOP 2.7.1 or later. For a staged
upgrade only, the client option `dsomop.dp.allow_legacy_servers`
(default `FALSE`) can be set to `TRUE` to permit legacy v2 servers,
including mixed federations. Every legacy server is named in a warning:
no first-answer binding: an unrotated data refresh can reveal whether a
released statistic changed; see isglobal-brge/dsOMOP\#20. The semantics
below apply to v3 sites. Public request identity is server-owned and
binds authenticated canonical dataset/recipe lineage, the typed
statistic and mechanism contract, public `snapshot_id`, and privacy
epoch. The analyst's `population_id` compatibility label and server
symbol alias do not participate. Servers 2.10.0 (seed-only) derive the
answer from that identity, the private bounded-statistic fingerprint and
a private automatic database version and store nothing: a reload that
adds rows re-draws every answer. Servers 2.7.1 to 2.9.x (first-answer)
store the first answer and return it to valid later requests even after
unrotated source updates, until the custodian rotates the snapshot or
epoch; their answers can remain stale, and different requests first
answered at different times need not represent one coherent source
snapshot. For multiple sites, pooling a non-count statistic additionally
requires one compatible public dsOMOP harmonization contract for age
grids, date semantics, calendar-day granularity, UTC handling, week
start, and operational caps. Per-site output and pooled distinct-person
counts do not depend on those value semantics and therefore do not
require that unrelated contract. Every input must have been produced by
an audited person-local server path and carry its authenticated
content-bound provenance capsule.

Legacy per-release answers report their server's fixed epsilon and delta
zero under that older contract. There is no lifetime privacy budget or
privacy call quota. Composition metadata describes only the
participating sites in the current federated release. Repeating a
question adds no observation; no full temporal transcript DP or timing,
admission, or rotation guarantee is claimed.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- omop_privacy("count")
ds.omop.dp.release("analysis_table", p)
} # }
```
