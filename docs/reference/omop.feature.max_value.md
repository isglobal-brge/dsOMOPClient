# Create a maximum value feature specification

Produces a feature spec that computes the maximum of a numeric column
across all records matching the concept set for each person. Useful for
identifying peak values (e.g. maximum creatinine, highest recorded
temperature).

## Usage

``` r
omop.feature.max_value(
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

  Character; name of the numeric column from which to find the maximum
  (default `"value_as_number"`).

- name:

  Character; optional custom name for the feature column. If `NULL`,
  auto-generated from the concept and table context.

## Value

An `omop_feature_spec` object with `type = "max_value"`.

## See also

[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md),
[`omop.feature.min_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.min_value.md),
[`omop.feature.mean_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.mean_value.md)

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.max_value(c(3020891),
  value_column = "value_as_number")
plan <- ds.omop.plan.features(plan, "labs",
  "measurement", specs = list(peak_creatinine = spec))
} # }
```
