# Create an age variable

Produces a derived variable that computes age from `year_of_birth`. With
`reference = "today"`, the constructor records today's ISO date in the
recipe so later executions remain reproducible. With
`reference = "index"`, age is computed relative to the cohort start
date.

## Usage

``` r
omop_variable_age(
  name = "age",
  reference = c("today", "index"),
  reference_date = NULL,
  year = NULL
)
```

## Arguments

- name:

  Character; output column name (default `"age"`).

- reference:

  Character; `"today"` or `"index"`.

- reference_date:

  Date or `NULL`; explicit reference date (overrides `reference`). Only
  the year is used for age.

- year:

  Integer or `NULL`; convenience shorthand for `reference_date` when you
  only care about a data-collection year (e.g. `year = 2024`). Overrides
  `reference`.

## Value

An `omop_variable` object with `format = "age"` and a `$derived`
metadata field.

## See also

[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md),
[`omop_variable_sex`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_sex.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(variables = omop_variable_age(),
                      outputs = omop_output(type = "wide"))
} # }
```
