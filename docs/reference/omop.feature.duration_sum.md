# Create a duration sum feature specification

Sums `end_date - start_date` (in days) for each person across all
records in tables that have start/end date columns.

## Usage

``` r
omop.feature.duration_sum(concept_set, name = NULL)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs.

- name:

  Character; optional custom column name.

## Value

An `omop_feature_spec` with `type = "duration_sum"`.
