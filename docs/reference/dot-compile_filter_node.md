# Compile a single filter or filter group into the server filter DSL

Compile a single filter or filter group into the server filter DSL

## Usage

``` r
.compile_filter_node(f, level = NULL, table = NULL)
```

## Arguments

- f:

  An `omop_filter` or `omop_filter_group` object.

- level:

  Character or `NULL`; if provided, filters at other levels are skipped.

- table:

  Character or `NULL`; source OMOP table.

## Value

A nested list node for the filter tree, or `NULL`.
