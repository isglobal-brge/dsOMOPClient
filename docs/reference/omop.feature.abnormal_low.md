# Create an abnormal-low count feature specification

Counts records where `value_as_number < range_low` per person.

## Usage

``` r
omop.feature.abnormal_low(concept_set, name = NULL)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs.

- name:

  Character; optional custom column name.

## Value

An `omop_feature_spec` with `type = "abnormal_low"`.
