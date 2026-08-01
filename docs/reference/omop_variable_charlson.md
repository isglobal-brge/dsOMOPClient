# Create a Charlson Comorbidity Index variable

Produces a derived variable computing the Charlson Comorbidity Index (17
categories, standard weights 1-6).

## Usage

``` r
omop_variable_charlson(name = "charlson")
```

## Arguments

- name:

  Character; output column name (default `"charlson"`).

## Value

An `omop_variable` with `format = "charlson"`.
