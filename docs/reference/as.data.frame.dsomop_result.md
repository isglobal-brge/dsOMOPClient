# Convert dsomop_result to data.frame

Extracts a single data frame from a `dsomop_result` object. If a pooled
result is available and is a data frame, it is returned. Otherwise, the
first server's result is used only for a per-site result. A pooled-scope
result whose pooled value is `NULL` returns an empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) so a failed
strict federation cannot silently degrade to one server. Returns an
empty [`data.frame()`](https://rdrr.io/r/base/data.frame.html) if no
valid data frame is found.

## Usage

``` r
# S3 method for class 'dsomop_result'
as.data.frame(x, ...)
```

## Arguments

- x:

  A `dsomop_result` object.

- ...:

  Additional arguments (ignored).

## Value

A data frame: the pooled result, the first server's result for per-site
scope, or an empty data frame as fallback.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- ds.omop.achilles.results(analysis_ids = 1, scope = "pooled")
df <- as.data.frame(res)
head(df)
} # }
```
