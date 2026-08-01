# Create a most-recent-value feature specification

Produces a feature spec that extracts the value from the most recent
(latest) record matching the concept set. Ordered by the table's date
column, the last record's `value_column` is returned. Commonly used for
lab measurements where the latest reading is clinically relevant (e.g.
most recent HbA1c).

## Usage

``` r
omop.feature.latest_value(
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

  Character; name of the column from which to extract the value (default
  `"value_as_number"`).

- name:

  Character; optional custom name for the feature column. If `NULL`,
  auto-generated from the concept and table context.

## Value

An `omop_feature_spec` object with `type = "latest_value"`.

## See also

[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md),
[`omop.feature.first_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.first_value.md),
[`omop.feature.mean_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.mean_value.md)

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.latest_value(c(3004410),
  value_column = "value_as_number")
plan <- ds.omop.plan.features(plan, "labs",
  "measurement", specs = list(latest_glucose = spec))
} # }
```
