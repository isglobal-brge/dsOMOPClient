# A study in one call (simple layer)

`omop_study()` returns an ordinary `omop_recipe`: one indexed population
(id `name`) and the requested outputs over it. It is exactly the
advanced recipe below (`recipe_to_code()` prints it; `print()` lists
every default the study chose):

  - population: `omop_population(name, index = omop_index(index, select
    = select), filters = <require>, coverage = coverage)`; when `index`
    is an `omop_population` (e.g. from `omop_new_users`) or an
    `omop_index`, its index and criteria are kept and the requirements
    are ANDed after them.

  - `require`: `prior_observation = n` is `omop_observed(before = n)`,
    `followup = n` is `omop_observed(after = n)`, `age = c(min, max)` is
    `omop_age_is(min, max)` (`NA` = open), `sex = "F"` / `"M"` is
    `omop_person(omop_where(gender_concept_id == 8532))` (8507), `has =
    list(<events>)` is `omop_has(e, window = omop_window(NULL, 0))` (any
    time up to the index date), `no_history_of = list(<events>)` is
    `omop_has_not(e, window = omop_window(NULL, -1))` (any time before
    the index date), `in_cohort = id` is `omop_in_cohort(id)`; criterion
    objects are used as they are; all in the order given.

  - `"baseline"`: a `wide` output with episode grain and every baseline
    variable (nested lists such as `omop_history()` are spliced in).

  - `"survival"`: a `survival` output (`competing_risk` with two or more
    outcomes) with `tar = omop_window(start, end)` and the `censor` list
    of `follow_up`.

  - `"long"`: a `long` output with the outcome events as sources
    (relative dates).

  - `"panel"`: a dense `person_period` output with 30-day bins over the
    follow-up window, counting the outcomes.

The simple layer never exposes grain, ties or date handling: every
output has episode grain, survival ties use the priority rule and dates
are relative to the index; `recipe_explain()` states them. A
`"survival"`, `"long"` or `"panel"` output without `outcomes`, or a
`"baseline"` output without `baseline` variables, is a structural error.

## Usage

``` r
omop_study(
  index,
  select = c("first", "first_eligible", "all", "last"),
  require = list(),
  baseline = list(),
  outcomes = list(),
  follow_up = list(start = 1L, end = NULL, censor = c("observation_end", "death")),
  outputs = c("baseline", "survival"),
  name = "study",
  coverage = omop_coverage()
)
```

## Arguments

  - index:

    An `omop_event`, `omop_era`, `list(cohort_definition_id = )`,
    `omop_any_of()`, an `omop_index()` or an indexed `omop_population`.

  - select:

    Index selection: `"first"` (default), `"first_eligible"`, `"all"` or
    `"last"`.

  - require:

    Named list of requirements (see Details).

  - baseline:

    Named list of baseline variables.

  - outcomes:

    Named list of outcome events.

  - follow\_up:

    `list(start = 1, end = NULL, censor = c("observation_end",
    "death"))`; give only the entries to change.

  - outputs:

    Subset of `"baseline"`, `"survival"`, `"long"`, `"panel"`.

  - name:

    Population name.

  - coverage:

    Observation coverage of the population (`omop_coverage`).

## Value

An `omop_recipe`.

## See also

`omop_new_users`, `omop_ever`, `omop_recipe`, `recipe_to_code`

## Examples

``` r
t2dm <- omop_concept_set(201826, descendants = TRUE, name = "t2dm")
metf <- omop_concept_set(1503297, descendants = TRUE, name = "metformin")
mi <- omop_concept_set(4329847, descendants = TRUE, name = "mi")
study <- omop_study(
  index    = omop_new_users(metf, washout_days = 365),
  require  = list(prior_observation = 365,
                  has = list(omop_event("condition_occurrence", t2dm))),
  baseline = list(age = omop_age(), sex = omop_sex(),
                  hba1c = omop_value_at_index("measurement", 3004410, unit = 8554),
                  inpatient = omop_count("visit_occurrence", 9201, before = 365),
                  charlson = omop_score("charlson")),
  outcomes = list(mi = omop_event("condition_occurrence", mi)),
  follow_up = list(start = 1, end = NULL, censor = c("observation_end", "death")))
print(study)
```
