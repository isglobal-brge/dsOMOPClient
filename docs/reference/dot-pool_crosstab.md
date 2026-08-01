# Pool a server cross-tab object by cell-wise summation

Each server returns a cross-tab list with `$counts` (a matrix whose
dimnames are row x col labels, NA for suppressed cells) or, when
stratified, `$stratified = TRUE` with `$strata` (a named list of
slices). This helper sums `n` per (row, col) cell key across sites. A
cell that is absent on any site (its row/col label not present) or NA
(suppressed) on any site is suppressed in the pooled table: under
`strict` it renders as `NA`; this fail-closed behaviour prevents
subtraction attacks.

## Usage

``` r
.pool_crosstab(per_site, policy = "strict")
```

## Arguments

- per_site:

  Named list of server cross-tab objects.

- policy:

  Character; "strict" or "pooled_only_ok".

## Value

List with \$result and \$warnings.
