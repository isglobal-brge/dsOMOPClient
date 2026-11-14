# Pool histograms by bin-wise sum

Pool histograms by bin-wise sum

## Usage

``` r
.pool_histograms(per_site_histograms, policy = "strict")
```

## Arguments

  - per\_site\_histograms:

    Named list of histogram data frames (each with bin\_start, bin\_end,
    count, suppressed)

  - policy:

    Character; pooling policy

## Value

List with $result (data.frame) and $warnings
