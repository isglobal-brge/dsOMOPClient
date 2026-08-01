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

- per_site_var:

  Named numeric vector of variances

- per_site_means:

  Named numeric vector of means

- per_site_counts:

  Named numeric vector of counts

- policy:

  Character; pooling policy

## Value

List with \$result and \$warnings
