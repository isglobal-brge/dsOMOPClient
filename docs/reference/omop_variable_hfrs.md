# Create an HFRS score variable

The Hospital Frailty Risk Score (analysis_id 926) uses ICD-10 source
codes mapped via concept_relationship to SNOMED targets (109 weighted
categories, decimal weights 0.1-7.1, binary presence x weight). Supports
both ICD10CM and ICD10 vocabularies. Returns 0 for all persons if
concept_relationship mappings are not available.

## Usage

``` r
omop_variable_hfrs(name = "hfrs")
```

## Arguments

- name:

  Character; output column name (default `"hfrs"`).

## Value

An `omop_variable` with `format = "hfrs"`.
