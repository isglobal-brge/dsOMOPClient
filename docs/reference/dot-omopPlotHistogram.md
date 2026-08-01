# Draw a federation-wide histogram from a value-histogram result

Sums the disclosure-safe, shared-edge bins across sites into one bar
chart. Used by `ds.omop.value.histogram(plot = TRUE)` so callers get a
plot directly instead of hand-combining per-site bins.

## Usage

``` r
.omopPlotHistogram(
  hist_result,
  nbins = 9L,
  xlab = NULL,
  main = NULL,
  col = "#4C72B0"
)
```
