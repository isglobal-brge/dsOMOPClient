# Matched and nested case-control sampling

A bounded, private, server-side risk-set sampler: for every case episode
(case time = the case's index date) eligible controls are the persons
satisfying the `controls` criteria population evaluated *at the case
time*, alive, at risk (no outcome on or before the case time under
`prior_outcome = "exclude"`), in the same strata; controls are taken in
a seeded hash order. The result population has the episodes `(person,
index = case time)` with `set_id`, `is_case` and `sampling_weight`
(`omop_population_column`).

## Usage

``` r
omop_match(
  cases,
  controls,
  outcome,
  at = "index",
  match = list(),
  controls_per_case = 4L,
  prior_outcome = c("exclude", "allow"),
  future_cases = c("eligible", "ineligible"),
  replacement = c("between_sets", "none"),
  seed = 1L,
  insufficient = c("keep_available", "drop_case"),
  missing_strata = c("ineligible", "match_missing"),
  max_candidates = 1e+06
)

omop_age_band(width = 5L)

omop_strata(event, before = NULL, window = NULL)
```

## Arguments

  - cases:

    Population id (or object) whose episodes are cases.

  - controls:

    Criteria population id (or object) used as a template.

  - outcome:

    Event giving the complete outcome history.

  - at:

    `"index"`.

  - match:

    List of `"sex"`, `"calendar_year"`, `omop_age_band()` and
    `omop_strata()` items.

  - controls\_per\_case:

    Controls per case.

  - prior\_outcome, future\_cases, replacement, insufficient,
    missing\_strata:

    Sampling policies (see the design).

  - seed:

    Non-negative integer seed.

  - max\_candidates:

    Cap on the candidate pairs (exceeding it is a typed refusal, never a
    truncation).

  - width:

    Age band width (years) at case time.

  - event:

    An event (strata presence before case time).

  - before:

    Strata: presence in the `before` days up to case time.

  - window:

    Strata: an explicit window around case time.

## Value

`omop_match`, or a match item for `match`.
