# Create a maximum gap (days) feature specification

Computes the maximum number of days between consecutive events per
person. Returns NA for persons with only one event.

## Usage

``` r
omop.feature.gap_max_days(concept_set, name = NULL)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs.

- name:

  Character; optional custom column name.

## Value

An `omop_feature_spec` with `type = "gap_max_days"`.
