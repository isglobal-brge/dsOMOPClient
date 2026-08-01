# Add an intervals (long) output to the plan

Extracts interval data (observation periods, visits, drug or condition
durations) with start and end days relative to the cohort index date.
Requires a cohort to be set. The output contains one row per matching
interval and cohort episode, with columns for table source, start day,
end day, and optionally concept IDs filtered by `concept_filter`. An
event that overlaps multiple recurrent cohort episodes can therefore
appear once for each matching episode, identified by `cohort_row_id`.

## Usage

``` r
ds.omop.plan.intervals(
  plan,
  tables = c("observation_period", "visit_occurrence", "drug_exposure",
    "condition_occurrence"),
  concept_filter = NULL,
  name = "intervals"
)
```

## Arguments

- plan:

  An `omop_plan` object.

- tables:

  Character vector; OMOP tables to extract intervals from. Defaults to
  observation_period, visit_occurrence, drug_exposure, and
  condition_occurrence.

- concept_filter:

  Named list; per-table concept ID filters where each element maps a
  table name to a numeric vector of concept IDs. If `NULL`, no concept
  filtering is applied.

- name:

  Character; output name used as a key in the plan's outputs list.

## Value

The modified `omop_plan` with the intervals output appended.

## See also

[`ds.omop.plan.events`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.events.md),
[`ds.omop.plan.temporal_covariates`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.temporal_covariates.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
plan <- ds.omop.plan.intervals(plan,
  tables = c("visit_occurrence", "drug_exposure"),
  concept_filter = list(drug_exposure = c(1127078, 1127433))
)
} # }
```
