# Create a mean value feature specification

Produces a feature spec that computes the arithmetic mean of a numeric
column across all records matching the concept set for each person.
Commonly used for averaging repeated lab measurements (e.g. mean
systolic blood pressure across visits).

## Usage

``` r
omop.feature.mean_value(
  concept_set,
  value_column = "value_as_number",
  name = NULL
)
```

## Arguments

  - concept\_set:

    Numeric vector of concept IDs, or an `omop_concept_set` object
    defining the concepts to match.

  - value\_column:

    Character; name of the numeric column to average (default
    `"value_as_number"`).

  - name:

    Character; optional custom name for the feature column. If `NULL`,
    auto-generated from the concept and table context.

## Value

An `omop_feature_spec` object with `type = "mean_value"`.

## See also

`ds.omop.plan.features`, `omop.feature.min_value`,
`omop.feature.max_value`

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.mean_value(c(3004249),
  value_column = "value_as_number")
plan <- ds.omop.plan.features(plan, "vitals",
  "measurement", specs = list(mean_sbp = spec))
} # }
```
