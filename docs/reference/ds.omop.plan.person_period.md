# Add a regular episode-by-period panel to an extraction plan

Declares the regular relative-time bins that intersect the unique OMOP
observation period covering each cohort index date. Event covariates are
stored sparsely; a missing `(rowId, timeId, covariateId)` row represents
zero only when that `(rowId, timeId)` exists in `personPeriods`. Each
roster row keeps the requested `startDay`/`endDay` bin and adds
inclusive `observationStartDay`, `observationEndDay`, and `daysObserved`
for the observed part of the bin. The output contains no absolute dates
or source event identifiers. It is a descriptive panel, not an inferred
risk set: use survival/counting-process output when cohort end, death,
or another censoring rule must define time at risk.

## Usage

``` r
ds.omop.plan.person_period(
  plan,
  table,
  concept_set = NULL,
  bin_width = 30L,
  window_start = -365L,
  window_end = 0L,
  analyses = c("binary"),
  grain = "episode",
  time_origin = "index",
  name = "person_period"
)
```

## Arguments

- plan:

  An `omop_plan` object.

- table:

  OMOP event table used for covariates.

- concept_set:

  Optional concept IDs or an OHDSI-style concept-set spec. `NULL`
  requests all observed concepts subject to the server cap.

- bin_width:

  Positive integer bin width in days.

- window_start, window_end:

  Inclusive integer days from index.

- analyses:

  Unique subset of `"binary"` and `"count"`.

- grain:

  Must be `"episode"`.

- time_origin:

  Must be `"index"`.

- name:

  Output name.

## Value

The modified `omop_plan`.

## See also

[`ds.omop.plan.temporal_covariates`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.temporal_covariates.md)
