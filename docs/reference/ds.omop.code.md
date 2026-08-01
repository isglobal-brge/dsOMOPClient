# Get the R code that produced a result

Extracts the stored R code string from a `dsomop_result` object, which
can be used to reproduce the analysis. The code is captured
automatically when each client wrapper function is called.

## Usage

``` r
ds.omop.code(x)
```

## Arguments

- x:

  A `dsomop_result` object.

## Value

Character string containing the reproducible R code. Returns an empty
string if no code was captured.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- ds.omop.achilles.results(analysis_ids = 1)
ds.omop.code(res)
} # }
```
