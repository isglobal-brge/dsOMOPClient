# Dispatch pooling by result type

Dispatch pooling by result type

## Usage

``` r
.pool_result(
  per_site,
  result_type,
  policy = "strict",
  harmonization = NULL,
  output_contract = NULL
)
```

## Arguments

- per_site:

  Named list of per-server results

- result_type:

  Character; one of the recognized result types

- policy:

  Character; "strict" or "pooled_only_ok"

- output_contract:

  Optional closed analysis output contract used instead of column-name
  heuristics for generic OHDSI-style frames.

## Value

List with \$result and \$warnings
