# Create an observation duration variable

Produces a derived variable that computes the number of days between
`observation_period_start_date` and `observation_period_end_date`.

## Usage

``` r
omop_variable_obs_duration(name = "obs_duration")
```

## Arguments

- name:

  Character; output column name (default `"obs_duration"`).

## Value

An `omop_variable` object with `format = "obs_duration"` and a
`$derived` metadata field.

## See also

[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md),
[`omop_variable_age`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_age.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(variables = omop_variable_obs_duration(),
                      outputs = omop_output(type = "wide"))
} # }
```
