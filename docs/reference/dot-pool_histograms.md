# Pool histograms by bin-wise sum

Bins are matched by their edges (`bin_start`, `bin_end`), the way date
counts are matched by period: servers drop suppressed bins, so positions
differ between sites. A bin absent or suppressed on a server is unknown
there, never zero. Servers whose bins overlap other servers' bins
without matching them use different edges and cannot be pooled.

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
