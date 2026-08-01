# Create a CHADS2 score variable

Produces a derived variable computing the CHADS2 stroke risk score
(analysis_id 903 in FeatureExtraction). Components: CHF, Hypertension,
Age \>= 75, Diabetes, Stroke/TIA (x2).

## Usage

``` r
omop_variable_chads2(name = "chads2", reference_date = NULL)
```

## Arguments

- name:

  Character; output column name (default `"chads2"`).

- reference_date:

  Date or `NULL`; date for the age component. When omitted, today's date
  is recorded at construction time.

## Value

An `omop_variable` with `format = "chads2"`.
