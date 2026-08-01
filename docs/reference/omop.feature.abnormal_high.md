# Create an abnormal-high count feature specification

Counts records where `value_as_number > range_high` per person.

## Usage

``` r
omop.feature.abnormal_high(concept_set, name = NULL)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs.

- name:

  Character; optional custom column name.

## Value

An `omop_feature_spec` with `type = "abnormal_high"`.
