# Warnings for pooled frames that mix standard and protected blocks

In standard mode a dsOMOP 2.10.3 server with a DP service fills the
person-time block (person-days, rates, MDRR, effects) of the C6/C9
entries from protected components and labels the frame `mixed`. The
established contract then sums or meta-analyses those fields like any
other: the pooled values are post-processing of protected (noisy)
values, not exact sums, and carry no pooled epsilon.

## Usage

``` r
.analysis_mixed_pool_warnings(raw)
```

## Arguments

  - raw:

    Named list of per-site frames.

## Value

Character vector of warnings (possibly empty).
