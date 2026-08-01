# Create a distinct concept count feature specification

Produces a feature spec that counts the number of distinct concept IDs
per person across all records in the table. Unlike other features, this
operates across all concepts rather than per-concept.

## Usage

``` r
omop.feature.n_distinct(concept_set = integer(0), name = NULL)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs (default `integer(0)`, meaning all
  concepts in the table).

- name:

  Character; optional custom name for the feature column.

## Value

An `omop_feature_spec` object with `type = "n_distinct"`.

## See also

[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md),
[`omop.feature.count`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.count.md)

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.n_distinct()
plan <- ds.omop.plan.features(plan, "diversity",
  "condition_occurrence", specs = list(n_conditions = spec))
} # }
```
