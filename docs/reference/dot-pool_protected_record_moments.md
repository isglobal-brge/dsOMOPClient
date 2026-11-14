# Pool record-weighted protected releases (capped record moments)

dsOMOP 2.10.3 releases the record-weighted QueryLibrary summaries as one
record-moment owner (family `RM`): per stratum the signed person total
N, the capped record count R and, on a public scale, the lattice sums S
and Q of the clipped, normalized record values. Compatible servers (same
entry, owner, cap, record order, lattice, normalization and allocation,
distinct logical sources) are pooled per stratum by summing N, R, S and
Q; the record-weighted mean and SD are derived once from the pooled R, S
and Q exactly as the server derives them (the persons never weight the
mean). Moments are pooled only when every contributing server released
them on the same public scale. Under `"strict"` only strata released by
every server are pooled; `"pooled_only_ok"` also pools strata released
by some servers (sums over those servers).

## Usage

``` r
.pool_protected_record_moments(payloads, policy = "strict", columns = NULL)
```

## Arguments

  - payloads:

    Named list of `dsomop_release` payloads.

  - policy:

    `"strict"` or `"pooled_only_ok"`.

  - columns:

    Optional contracted columns (their names are the schema).

## Value

List: `result` (the summary's own columns) and `warnings`.

## Details

The versioned `record-display-v1` projection holds displayed R in \[N, K
N\] before deriving moments. Compatible signed value histograms are
summed, then projected onto that displayed R. The result attributes
`value_histograms` and `value_quantiles` contain their cells and
interval quantiles, including open-tail censoring flags. A stratum's
histogram is available only if every contributing site released it on
the same public grid; missing moments never cause an exact-value
fallback.
