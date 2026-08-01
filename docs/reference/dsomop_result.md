# Create a dsomop_result object

Constructs a standardised `dsomop_result` S3 object that wraps every
dsOMOPClient function return value. The object stores per-site results,
an optional pooled (cross-server aggregated) result, and metadata
including the reproducible R code that produced the result, the
timestamp, and any pooling warnings.

## Usage

``` r
dsomop_result(per_site, pooled = NULL, meta = list())
```

## Arguments

- per_site:

  Named list mapping server names to their raw results (data frames,
  lists, or scalars).

- pooled:

  `NULL` (default) or a single aggregated result (typically a data
  frame) combining all servers.

- meta:

  Named list of metadata. Recognised elements: `call_code` (character;
  reproducible R code), `scope` (character; `"per_site"` or `"pooled"`),
  `pooling_policy` (character; `"strict"` or `"pooled_only_ok"`),
  `warnings` (character vector of pooling warnings).

## Value

A `dsomop_result` object (a list with class
`c("dsomop_result", "list")`).
