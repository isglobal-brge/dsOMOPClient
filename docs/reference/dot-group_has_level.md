# Check if a filter group contains any filter at a given level

Check if a filter group contains any filter at a given level

## Usage

``` r
.group_has_level(group, level)
```

## Arguments

- group:

  An `omop_filter_group` object.

- level:

  Character; the filter level to check for.

## Value

Logical; `TRUE` if any descendant filter matches the level.
