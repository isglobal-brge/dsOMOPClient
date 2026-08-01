# Print a dsomop_result

Prints the per-site result tables and the pooled (cross-server) result
table, followed by any disclosure/pooling warnings.

## Usage

``` r
# S3 method for class 'dsomop_result'
print(x, ...)
```

## Arguments

- x:

  A `dsomop_result` object.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns `x`.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- ds.omop.achilles.status()
print(res)
} # }
```
