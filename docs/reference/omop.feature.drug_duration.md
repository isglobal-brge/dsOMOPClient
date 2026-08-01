# Create a drug duration feature specification

Produces a feature spec that computes the duration of drug exposure
records (`drug_exposure_end_date - drug_exposure_start_date`) and
aggregates per person using the specified function.

## Usage

``` r
omop.feature.drug_duration(concept_set, agg = "mean", name = NULL)
```

## Arguments

- concept_set:

  Numeric vector of concept IDs, or an `omop_concept_set` object.

- agg:

  Character; aggregation function — `"mean"`, `"sum"`, or `"max"`
  (default `"mean"`).

- name:

  Character; optional custom name for the feature column.

## Value

An `omop_feature_spec` object with `type = "drug_duration"`.

## See also

[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md),
[`omop.feature.count`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.count.md)

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.drug_duration(c(1124300), agg = "mean")
} # }
```
