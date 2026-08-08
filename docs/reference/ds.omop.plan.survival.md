# Add a survival (time-to-event) output to the plan

The historical single-outcome call produces one row per cohort episode
with an event indicator and time-to-event in days. Advanced calls can
retain named endpoints as survival, competing-risk, recurrent-event, or
counting-process data. Calendar dates and source event identifiers are
never returned. Requires a cohort to be set. Historical plans without an
explicit censoring field are censored at the end of the observation
period containing the index episode; they never bridge an unobserved gap
to a later period.

## Usage

``` r
ds.omop.plan.survival(
  plan,
  outcome_table = "condition_occurrence",
  outcome_concepts = NULL,
  tar = list(start_offset = 0, end_offset = 730),
  event_order = "first",
  name = "survival",
  outcomes = NULL,
  censoring = NULL,
  format = NULL,
  washout_days = 0L,
  tie_policy = "priority"
)
```

## Arguments

- plan:

  An `omop_plan` object.

- outcome_table:

  Character; OMOP table containing outcome events (e.g.
  `"condition_occurrence"`, `"procedure_occurrence"`).

- outcome_concepts:

  Numeric vector; concept IDs defining the historical composite outcome.
  Omit when using \`outcomes\`.

- tar:

  Named list; time-at-risk window with `start_offset` and `end_offset`
  (integer days relative to cohort_start_date).

- event_order:

  Character; `"first"` or `"last"` to select which event occurrence
  determines the time-to-event value; advanced recurrent/counting
  formats also accept \`all\`.

- name:

  Character; output name used as a key in the plan's outputs list.

- outcomes:

  Named list of endpoint specifications. Each endpoint contains
  \`table\`, \`concept_set\`, and optional safe row \`filters\`.

- censoring:

  Named list controlling observation-period, death, cohort-end, and
  optional administrative-date censoring.

- format:

  Character; \`survival\`, \`competing_risk\`, \`recurrent_events\`, or
  \`counting_process\`.

- washout_days:

  Non-negative integer washout between events of the same named
  endpoint.

- tie_policy:

  Character; \`priority\`, \`error\`, or \`all\`. The latter is
  restricted to recurrent-event output.

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
