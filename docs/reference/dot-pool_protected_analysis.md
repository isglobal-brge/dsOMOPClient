# Pool the frames of an entry with protected components

Frames carrying protected components are pooled from their signed
components (`.pool_protected_release_frames`). When no server returned
protected components (each one's DP service is unavailable), the
standard outputs, whose disclosive fields are unavailable, are pooled
with each other by the entry's established contract
(`standard_contract`). An effect estimate (`effect_estimate` descriptor)
is pooled as in dsOMOP 2.10.2, by inverse-variance meta-analysis of the
per-site protected estimates and SEs: post-processing of the protected
releases, after the sites were checked to be compatible, distinct
logical sources. The pooled components stay available as attribute
`protected_components`.

## Usage

``` r
.pool_protected_analysis(
  raw,
  release_pooling,
  policy = "strict",
  harmonization = NULL
)
```

## Arguments

  - raw:

    Named list of per-site frames.

  - release\_pooling:

    The entry's `release_pooling` descriptor.

  - policy:

    Pooling policy.

  - harmonization:

    Harmonization contract.

## Value

List: `result`, `warnings`, `harmonization`.
