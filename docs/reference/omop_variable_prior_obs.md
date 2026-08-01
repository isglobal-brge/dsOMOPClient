# Create a prior observation duration variable

Produces a derived variable computing days from observation start to a
fixed reference date. When omitted, today's date is recorded in the
variable specification at construction time.

## Usage

``` r
omop_variable_prior_obs(name = "prior_obs", reference_date = NULL)
```

## Arguments

- name:

  Character; output column name (default `"prior_obs"`).

- reference_date:

  Date or `NULL`; explicit reference date.

## Value

An `omop_variable` with `format = "prior_obs"`.
