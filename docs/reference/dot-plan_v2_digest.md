# Semantic digest of a v2 plan over its normal form

The digest binds the federation harmonisation signature and the DP
provenance capsule: `.planNormalizedDigest()` of the vendored A-11
normaliser (R/plan\_normalize\_v2.R), i.e. SHA-256 of the RFC 8785
serialisation of the normalised plan without its advisory fields.

## Usage

``` r
.plan_v2_digest(plan)
```

## Arguments

  - plan:

    A v2 plan.

## Value

A list with `payload` and `sha256`.
