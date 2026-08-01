# Create a coefficient of variation feature specification

Produces a feature spec that computes `sd / mean * 100` per person.

## Usage

``` r
omop.feature.cv_value(
  concept_set,
  value_column = "value_as_number",
  name = NULL
)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs.

- value_column:

  Character; numeric column (default `"value_as_number"`).

- name:

  Character; optional custom column name.

## Value

An `omop_feature_spec` with `type = "cv_value"`.
