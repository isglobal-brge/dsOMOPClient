# Create a followup duration variable

Produces a derived variable computing days from a reference date
(default fixed reference date to observation end. When omitted, today's
date is recorded in the variable specification at construction time.
With multiple spells, the unique period containing the reference date is
selected.

## Usage

``` r
omop_variable_followup(name = "followup", reference_date = NULL)
```

## Arguments

- name:

  Character; output column name (default `"followup"`).

- reference_date:

  Date or `NULL`; explicit reference date.

## Value

An `omop_variable` with `format = "followup"`.
