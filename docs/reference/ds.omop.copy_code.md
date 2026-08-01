# Copy reproducible R code to clipboard

Extracts the stored R code from a `dsomop_result` object and copies it
to the system clipboard using the clipr package. If clipr is not
installed or clipboard access fails, the code is printed to the console
instead.

## Usage

``` r
ds.omop.copy_code(x)
```

## Arguments

- x:

  A `dsomop_result` object.

## Value

Invisibly returns the code string (character).

## Examples

``` r
if (FALSE) { # \dontrun{
res <- ds.omop.achilles.results(analysis_ids = 1)
ds.omop.copy_code(res)  # copies to clipboard
} # }
```
