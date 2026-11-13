# Dispatch pooling by result type

Dispatch pooling by result type

## Usage

``` r
.pool_result(per_site, result_type, policy = "strict", harmonization = NULL)
```

## Arguments

  - per\_site:

    Named list of per-server results

  - result\_type:

    Character; one of the recognized result types

  - policy:

    Character; "strict" or "pooled\_only\_ok"

## Value

List with $result and $warnings
