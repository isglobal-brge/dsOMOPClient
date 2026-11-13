# Refuse a recipe-level row filter on an output whose table lacks a column

Defect F-2: a recipe-level row filter is attached only to outputs whose
event table has every column the filter references; otherwise
compilation stops naming the output and the missing column. The
table-agnostic sentinels (`start_date`, `end_date`, `concept_id`) are
resolved per table by the server; a column the CDM does not define at
all (a site extension) is left to the server's own validation.

## Usage

``` r
.assert_row_filter_columns(tree, table, output_name)
```

## Arguments

  - tree:

    Compiled row-filter tree for the output's table.

  - table:

    The output's event table.

  - output\_name:

    The plan output name.

## Value

`TRUE` invisibly.
