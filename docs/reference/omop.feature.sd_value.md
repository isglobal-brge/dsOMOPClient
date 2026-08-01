# Create a standard deviation feature specification

Produces a feature spec that computes the standard deviation of a
numeric column per person. Requires at least 2 values per person;
returns NA otherwise.

## Usage

``` r
omop.feature.sd_value(
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

An `omop_feature_spec` with `type = "sd_value"`.
