# Compile population filters into the server filter-tree DSL

Compile population filters into the server filter-tree DSL

## Usage

``` r
.compile_population_filter_tree(filters, default_operator = "and")
```

## Arguments

- filters:

  List of population-level `omop_filter` or `omop_filter_group` objects.

- default_operator:

  Character; default Boolean operator (`"and"`).

## Value

A nested population filter tree, or `NULL` if empty.
