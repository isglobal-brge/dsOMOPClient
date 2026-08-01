# Recursively flatten filter groups into individual filters at a given level

Recursively flatten filter groups into individual filters at a given
level

## Usage

``` r
.flatten_filters(filters, level = NULL)
```

## Arguments

- filters:

  List of `omop_filter` and/or `omop_filter_group` objects.

- level:

  Character or `NULL`; filter level to keep (`NULL` keeps all).

## Value

Flat list of `omop_filter` objects.
