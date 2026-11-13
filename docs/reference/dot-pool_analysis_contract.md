# Pool an analysis-catalog result using only its server-owned contract

Pool an analysis-catalog result using only its server-owned contract

## Usage

``` r
.pool_analysis_contract(
  per_site,
  contract,
  policy = c("strict", "pooled_only_ok"),
  harmonization = NULL
)
```

## Arguments

  - per\_site:

    Named per-server data frames.

  - contract:

    Valid pooling contract from analysis metadata.

  - policy:

    Suppression policy: `"strict"` or `"pooled_only_ok"`.

  - harmonization:

    Optional negotiated federation contract.

## Value

List with `result`, `warnings`, and `harmonization`.
