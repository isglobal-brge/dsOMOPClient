# Request a sticky privacy release

Performs a complete-federation preflight, requests the same typed
release from every node, verifies the returned mechanism contract, and
optionally pools only the noisy sufficient statistics. A failure at any
site stops the call without publishing another site's value. Servers may
already have committed their sticky release; retrying the identical
request returns the same noise rather than rerolling it. Sticky release
identity is server-owned and bound to authenticated canonical
dataset/recipe lineage, the typed statistic, and the custodian-owned
public `snapshot_id`. A separate private fingerprint detects drift in
the bounded sufficient statistic; protected values never select the
noise draw. The analyst's `population_id` compatibility label and server
symbol alias do not participate, so changing either does not request or
guarantee fresh noise. The custodian must rotate `snapshot_id` when the
protected ETL snapshot changes; that controlled rotation intentionally
creates a new release identity. For multiple sites, pooling a non-count
statistic additionally requires one compatible public dsOMOP
harmonization contract for age grids, date semantics, calendar-day
granularity, UTC handling, week start, and operational caps. Per-site
output and pooled distinct-person counts do not depend on those value
semantics and therefore do not require that unrelated contract. Every
input must have been produced by an audited person-local server path and
carry its authenticated content-bound provenance capsule.

## Usage

``` r
ds.omop.dp.release(
  x,
  privacy,
  datasources = NULL,
  pool = TRUE,
  format = c("long", "wide", "vector", "raw")
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

## Value

A `dsomop_result`. The `meta$privacy` record reports the effective
population label, a named public snapshot map, named per-server
accounting records, nominal per-site epsilon, degradation, and
conservative cross-site accounting. A pooled payload is marked degraded
if any site returned its data-independent fallback. Parallel accounting
is used only when every server explicitly attests
`disjoint_persons = TRUE`. Without that attestation, pooling sums
site-local contributions and may count the same real person once per
site; the privacy loss is composed sequentially. Multi-site results also
carry `meta$harmonization` when non-count values were pooled.

## Details

No accounting mode hard-blocks a new operation. In `"bounded_accounted"`
mode, the nominal noise calibration follows a summable server-owned
schedule; once an informative allocation is too small, the endpoint
returns a marked, data-independent degraded payload at epsilon zero. In
`"sticky_unbounded"` mode, an exact authenticated canonical lineage and
statistic cannot be rerolled. Alternate constructions that happen to be
mathematically equivalent may still be distinct releases, and unlimited
distinct queries do not have a finite global DP composition guarantee.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- omop_privacy("count")
ds.omop.dp.release("analysis_table", p)
} # }
```
