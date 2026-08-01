# Extract filters (or groups containing filters) at a given level

Extract filters (or groups containing filters) at a given level

## Usage

``` r
.extract_filters_by_level(filters, level)
```

## Arguments

- filters:

  List of `omop_filter` and/or `omop_filter_group` objects.

- level:

  Character; the filter level to extract (`"population"`, `"row"`, or
  `"output"`).

## Value

List of matching `omop_filter` or `omop_filter_group` objects.
