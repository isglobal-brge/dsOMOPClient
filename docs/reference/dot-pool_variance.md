# Pool variance using Cochrane formula

Pool variance using Cochrane formula

## Usage

``` r
.pool_variance(
  per_site_var,
  per_site_means,
  per_site_counts,
  policy = "strict"
)
```

## Arguments

  - per\_site\_var:

    Named numeric vector of variances

  - per\_site\_means:

    Named numeric vector of means

  - per\_site\_counts:

    Named numeric vector of counts

  - policy:

    Character; pooling policy

## Value

List with $result and $warnings
