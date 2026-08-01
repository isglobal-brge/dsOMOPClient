# Create a minimum value feature specification

Produces a feature spec that computes the minimum of a numeric column
across all records matching the concept set for each person. Useful for
identifying the lowest recorded value (e.g. nadir hemoglobin, minimum
blood pressure).

## Usage

``` r
omop.feature.min_value(
  concept_set,
  value_column = "value_as_number",
  name = NULL
)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs, or an `omop_concept_set` object
  defining the concepts to match.

- value_column:

  Character; name of the numeric column from which to find the minimum
  (default `"value_as_number"`).

- name:

  Character; optional custom name for the feature column. If `NULL`,
  auto-generated from the concept and table context.

## Value

An `omop_feature_spec` object with `type = "min_value"`.

## See also

[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md),
[`omop.feature.max_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.max_value.md),
[`omop.feature.mean_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.mean_value.md)

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.min_value(c(3000963),
  value_column = "value_as_number")
plan <- ds.omop.plan.features(plan, "labs",
  "measurement", specs = list(min_hemoglobin = spec))
} # }
```
