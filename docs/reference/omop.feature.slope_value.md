# Create a slope (linear trend) feature specification

Produces a feature spec that fits a linear model of value over time per
person and extracts the slope coefficient. Requires at least 2 data
points.

## Usage

``` r
omop.feature.slope_value(
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

An `omop_feature_spec` with `type = "slope_value"`.
