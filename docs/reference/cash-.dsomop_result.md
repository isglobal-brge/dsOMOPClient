# Access dsomop\_result elements with backward compatibility

Custom `$` operator for `dsomop_result` objects. Top-level fields
(`per_site`, `pooled`, `meta`) are returned directly. Any other name
falls through to the `per_site` list, allowing backward-compatible
access patterns such as `result$server_a` instead of
`result$per_site$server_a`.

## Usage

``` r
# S3 method for class 'dsomop_result'
x$name

# S3 method for class 'dsomop_result'
x[[i, exact = TRUE]]

# S3 method for class 'dsomop_listing'
x$name

# S3 method for class 'dsomop_listing'
x[[i, exact = TRUE]]
```

## Arguments

  - x:
    
    A `dsomop_result` object.

  - name:
    
    Character; the element name to access.

  - i:
    
    Element name or position, as for `[[`.

  - exact:
    
    Passed to `[[`.

## Value

The requested element: a top-level field, or the matching entry from
`per_site`, or `NULL` if not found.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- ds.omop.achilles.status()
res$per_site          # top-level access
res$server_a          # falls through to per_site[["server_a"]]
} # }
```
