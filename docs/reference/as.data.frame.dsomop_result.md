# Convert dsomop\_result to data.frame

Extracts one data frame from a `dsomop_result` object. A split result
(`type = "split"`) stacks the per-server data frames into one frame with
a leading `server` column, so no site is dropped silently. A `"combine"`
or `"both"` result returns its pooled data frame; when no pooled value
could be formed, it returns an empty `data.frame()` so a failed strict
federation never degrades to a partial view (use `type = "split"` or
`$per_site` for the per-server results).

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

A data frame: the pooled result, the stacked per-server results with a
`server` column for split results, or an empty data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- ds.omop.achilles.results(analysis_ids = 1, type = "both")
df <- as.data.frame(res)
head(df)
} # }
```
