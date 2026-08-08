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
  filters = NULL,
  window = NULL,
  interval_match = "overlaps",
  event_select = "all",
  select_n = 1L,
  select_by = "episode_source",
  anchor = 0L,
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

  Named list; each table maps to concept IDs or a standard concept-set
  specification with `concepts`, optional descendant/mapped expansion,
  and exclusions. If `NULL`, no concept filtering is applied.

- filters:

  Optional uniquely named per-table list of reviewed filter DSL trees.
  Each tree applies only to its named source table.

- window:

  Optional index-relative window. Supply start/end offsets for overlap,
  start, or end matching, or an at offset for active-at matching.

- interval_match:

  Interval relationship: `"overlaps"`, `"starts_in"`, `"ends_in"`, or
  `"active_at"`. Without an explicit window, matching is against the
  cohort episode itself.

- event_select:

  Repeated-event policy: `"all"`, `"first"`, `"last"`, or `"nearest"`.

- select_n:

  Positive number of intervals retained per selection group.

- select_by:

  Group selection by episode and source, optionally also by concept.

- anchor:

  Integer days from index used by nearest-event selection.

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
