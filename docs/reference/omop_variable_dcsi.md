# Create a DCSI score variable

The Diabetes Complications Severity Index (analysis\_id 902) uses ICD9CM
source codes mapped via concept\_relationship to SNOMED targets (tiered
scoring: MAX tier per category, SUM across 7 categories, max total 13).
Requires ICD9CM vocabulary loaded in the CDM. Returns 0 for all persons
if concept\_relationship mappings are not available.

## Usage

``` r
omop_variable_dcsi(name = "dcsi")
```

## Arguments

  - name:

    Character; output column name (default `"dcsi"`).

## Value

An `omop_variable` with `format = "dcsi"`.
