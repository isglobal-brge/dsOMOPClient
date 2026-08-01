# Create an event count feature specification

Produces a feature spec that generates an integer column containing the
number of records per person matching the concept set. Useful for
quantifying utilisation (e.g. number of visits, number of drug
dispensings).

## Usage

``` r
omop.feature.count(concept_set, name = NULL)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs, or an `omop_concept_set` object
  defining the concepts to count.

- name:

  Character; optional custom name for the feature column. If `NULL`,
  auto-generated from the concept and table context.

## Value

An `omop_feature_spec` object with `type = "count"`.

## See also

[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md),
[`omop.feature.boolean`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.boolean.md),
[`omop.feature.mean_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.mean_value.md)

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.count(c(9201, 9202, 9203))
plan <- ds.omop.plan.features(plan, "visit_counts",
  "visit_occurrence", specs = list(n_visits = spec))
} # }
```
