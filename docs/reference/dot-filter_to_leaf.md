# Convert an omop_filter to a leaf node matching the server's .compileFilter()

Convert an omop_filter to a leaf node matching the server's
.compileFilter()

## Usage

``` r
.filter_to_leaf(f, table = NULL)
```

## Arguments

- f:

  An `omop_filter` object.

- table:

  Character or `NULL`; OMOP table used for standard date column
  inference.

## Value

A list with `var`, `op`, and `value` fields, or an error when the filter
cannot be represented faithfully.
