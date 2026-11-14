# Medical history covariates (simple layer)

One binary variable per item: 1 when the condition, drug or procedure
was recorded in the `days_before` days up to and including the index
date (`omop_ever(table, item, before = days_before)`). Each argument is
a vector of concept IDs (one variable per ID; vector names name the
variables) or a named list of concept IDs / `omop_concept_set`s (one
variable per element). Unnamed items are named
`<condition|drug|procedure>_<concept_id>`.

## Usage

``` r
omop_history(
  condition = NULL,
  drug = NULL,
  procedure = NULL,
  days_before = 365L
)
```

## Arguments

  - condition, drug, procedure:

    Items over `condition_occurrence`, `drug_exposure` and
    `procedure_occurrence`.

  - days\_before:

    Lookback in days (`Inf`: any time up to the index).

## Value

A named list of `omop_variable`s, for `omop_study(baseline = )` or
`omop_recipe(variables = )`.

## See also

`omop_ever`, `omop_study`

## Examples

``` r
omop_history(condition = c(diabetes = 201820, hypertension = 320128),
             drug = list(statins = omop_concept_set(1539403, descendants = TRUE)))
```
