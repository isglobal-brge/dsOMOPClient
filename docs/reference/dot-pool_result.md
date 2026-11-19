# Dispatch pooling by result type

Dispatch pooling by result type

## Usage

``` r
.pool_result(
  per_site,
  result_type,
  policy = "strict",
  harmonization = NULL,
  options = list()
)
```

## Arguments

  - per\_site:
    
    Named list of per-server results

  - result\_type:
    
    Character; one of the recognized result types

  - policy:
    
    Character; "strict" or "pooled\_only\_ok"

  - harmonization:
    
    Optional negotiated federation contract.

  - options:
    
    Named list of request options that shape the pooled view (`metric`
    and `top_n` for concept prevalence).

## Value

List with $result and $warnings
