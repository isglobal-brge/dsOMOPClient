# Create a binary (presence/absence) feature specification

Produces a feature spec that generates a boolean column indicating
whether the person has any records matching the concept set. When used
in a plan or recipe, the resulting column will contain `TRUE` (at least
one matching record) or `FALSE` (no matching records).

## Usage

``` r
omop.feature.boolean(concept_set, name = NULL)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs, or an `omop_concept_set` object
  defining the concepts to match.

- name:

  Character; optional custom name for the feature column. If `NULL`,
  auto-generated from the concept and table context.

## Value

An `omop_feature_spec` object with `type = "boolean"`.

## See also

[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md),
[`omop.feature.count`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.count.md),
[`omop.feature.mean_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.mean_value.md)

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.boolean(c(201826))
plan <- ds.omop.plan.features(plan, "has_diabetes",
  "condition_occurrence", specs = list(diabetes = spec))
} # }
```
