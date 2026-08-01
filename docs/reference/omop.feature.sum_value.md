# Create a sum value feature specification

Produces a feature spec that sums a numeric column across all records
matching the concept set for each person. Useful for computing total
days supply, total quantity, etc.

## Usage

``` r
omop.feature.sum_value(concept_set, value_column = "days_supply", name = NULL)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs, or an `omop_concept_set` object.

- value_column:

  Character; name of the numeric column to sum (default
  `"days_supply"`).

- name:

  Character; optional custom name for the feature column.

## Value

An `omop_feature_spec` object with `type = "sum_value"`.

## See also

[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md),
[`omop.feature.mean_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.mean_value.md)

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.sum_value(c(1124300),
  value_column = "days_supply")
} # }
```
