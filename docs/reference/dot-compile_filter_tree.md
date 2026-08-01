# Compile a list of filters into an AND/OR tree for the server filter DSL

Compile a list of filters into an AND/OR tree for the server filter DSL

## Usage

``` r
.compile_filter_tree(
  filters,
  default_operator = "and",
  level = NULL,
  table = NULL
)
```

## Arguments

- filters:

  List of `omop_filter` or `omop_filter_group` objects.

- default_operator:

  Character; default Boolean operator (`"and"`).

- level:

  Character or `NULL`; if provided, filters at other levels are skipped
  while preserving valid descendants inside groups.

- table:

  Character or `NULL`; OMOP table used to infer standard columns for
  table-dependent row filters such as `date_range`.

## Value

A nested list structure representing the filter tree, or `NULL` if
empty.
