# New users of a drug (simple layer)

The population of first-time users after a washout: drug exposures of
`drug` are grouped into eras (`omop_era`, 30-day gap, `by`), the index
is the era start selected by `select` (default `"first_eligible"`: the
earliest era that meets the criteria), the episode ends at the era end
(`omop_end("interval_end")`), and the criteria require `washout_days`
days of prior observation (`omop_observed(before = washout_days)`) and
no exposure in the `washout_days` days before the index
(`omop_has_not(rx, window = omop_window(-washout_days, -1))`).
`washout_days = 0` adds no criteria. A parameterized washout is expanded
after binding or default resolution; zero keeps independently supplied
population and study requirements.

## Usage

``` r
omop_new_users(
  drug,
  washout_days = 365L,
  select = "first_eligible",
  by = "ingredient"
)
```

## Arguments

  - drug:

    Concept IDs, an `omop_concept_set`, the name of a registry concept
    set, or an `omop_event()` over `drug_exposure`.

  - washout\_days:

    Washout in days (a non-negative integer).

  - select:

    Index selection (see `omop_index`).

  - by:

    Era grouping: `"ingredient"` (default), `"concept"` or `"event"`.

## Value

An `omop_population` with id `"new_users"` (rename with `p$id <- ...`);
`omop_study` uses it as its index and ANDs its own requirements.

## See also

`omop_study`, `omop_era`, `omop_index`

## Examples

``` r
metf <- omop_concept_set(1503297, descendants = TRUE, name = "metformin")
omop_new_users(metf, washout_days = 365)
```
