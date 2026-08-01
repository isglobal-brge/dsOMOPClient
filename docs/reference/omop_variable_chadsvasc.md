# Create a CHA2DS2-VASc score variable

Produces a derived variable computing the CHA2DS2-VASc stroke risk score
(7 categories for atrial fibrillation).

## Usage

``` r
omop_variable_chadsvasc(name = "chadsvasc", reference_date = NULL)
```

## Arguments

- name:

  Character; output column name (default `"chadsvasc"`).

- reference_date:

  Date or `NULL`; date for the age component. When omitted, today's date
  is recorded at construction time.

## Value

An `omop_variable` with `format = "chadsvasc"`.
