# Create a drug duration variable

Produces a feature variable that computes the duration of drug exposures
(`drug_exposure_end_date - drug_exposure_start_date`) and aggregates per
person using the specified function.

## Usage

``` r
omop_variable_drug_duration(
  concept_id,
  concept_name = NULL,
  name = NULL,
  agg = c("mean", "sum", "max")
)
```

## Arguments

- concept_id:

  Integer; drug concept ID.

- concept_name:

  Character or `NULL`; human-readable name.

- name:

  Character or `NULL`; output column name (auto-generated if `NULL`).

- agg:

  Character; aggregation function — `"mean"`, `"sum"`, or `"max"`.

## Value

An `omop_variable` object with `format = "drug_duration"` and a
`$derived` metadata field.

## See also

[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md),
[`omop.feature.drug_duration`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.drug_duration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  variables = omop_variable_drug_duration(1124300, concept_name = "Metformin"),
  outputs = omop_output(type = "wide"))
} # }
```
