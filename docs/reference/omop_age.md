# Person-derived and population-column variables

Helpers that return ready variables (their name is taken from the list
element name in `omop_recipe(variables = list(age = omop_age()))`, or a
default).

## Usage

``` r
omop_age(
  reference = c("index", "today"),
  year = NULL,
  reference_date = NULL,
  bands = FALSE,
  age_breaks = NULL
)

omop_sex()

omop_race()

omop_ethnicity()

omop_birth_year_band(width = 5)

omop_died(before = NULL)

omop_days_to_death()

omop_n_observation_periods()

omop_obs_duration(period_policy = c("total", "first", "last", "longest"))

omop_prior_obs(reference_date)

omop_followup(reference_date)

omop_observed_days(window)

omop_score(type, reference_date = NULL)

omop_population_column(
  name = c("index_source", "set_id", "is_case", "sampling_weight")
)
```

## Arguments

  - reference:

    `"index"` or `"today"` (today's date is recorded at construction so
    executions stay reproducible).

  - year:

    Optional reference year (shorthand for `reference_date`).

  - reference\_date:

    Optional fixed ISO reference date.

  - bands:

    Logical; legacy `age_group` bands.

  - age\_breaks:

    `NULL` or integer band lower bounds starting at 0.

  - width:

    Birth-year band width (years).

  - before:

    Optional day offset: died before index + `before`.

  - period\_policy:

    Observation-duration policy.

  - window:

    An `omop_window()` for `omop_observed_days()`.

  - type:

    Score type.

  - name:

    Population column: `"index_source"`, `"set_id"`, `"is_case"` or
    `"sampling_weight"`.

## Value

An `omop_variable`.

## Details

`omop_age()` is the annual age (reference year minus year of birth) at
the index date (or today / a fixed `reference_date` or `year`); `bands =
TRUE` gives the legacy `age_group` on the server's default grid or on
`age_breaks` (any strictly increasing integer lower bounds starting at
0). Exact age and exact birth components are ordinary quasi-identifier
columns on assign paths (released only through the final minimum-persons
rule). `omop_score()` computes a comorbidity score (charlson, chads2,
chadsvasc, dcsi, hfrs) over conditions on or before the reference date
(the index when `NULL`).

## Examples

``` r
omop_age(); omop_age(bands = TRUE, age_breaks = c(0, 18, 65))
omop_score("charlson"); omop_observed_days(omop_window(-365, 0))
```
