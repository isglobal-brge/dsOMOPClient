# Add a survival (time-to-event) output to the plan

Produces one row per cohort episode with an event indicator (0/1) and
time-to-event in days. Calendar dates are omitted; the assigned
server-side object remains subject to the ordinary DataSHIELD disclosure
controls. Requires a cohort to be set.

## Usage

``` r
ds.omop.plan.survival(
  plan,
  outcome_table = "condition_occurrence",
  outcome_concepts,
  tar = list(start_offset = 0, end_offset = 730),
  event_order = "first",
  name = "survival"
)
```

## Arguments

- plan:

  An `omop_plan` object.

- outcome_table:

  Character; OMOP table containing outcome events (e.g.
  `"condition_occurrence"`, `"procedure_occurrence"`).

- outcome_concepts:

  Numeric vector; concept IDs that define the outcome event.

- tar:

  Named list; time-at-risk window with `start_offset` and `end_offset`
  (integer days relative to cohort_start_date).

- event_order:

  Character; `"first"` or `"last"` to select which event occurrence
  determines the time-to-event value.

- name:

  Character; output name used as a key in the plan's outputs list.

## Value

The modified `omop_plan` with the survival output appended.

## See also

[`ds.omop.plan.events`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.events.md),
[`ds.omop.plan.cohort`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.cohort.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
plan <- ds.omop.plan.survival(plan,
  outcome_table = "condition_occurrence",
  outcome_concepts = c(201826, 443238),
  tar = list(start_offset = 0, end_offset = 365),
  event_order = "first"
)
} # }
```
