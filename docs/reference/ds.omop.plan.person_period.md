# Add a regular episode-by-period panel to an extraction plan

Declares a complete roster of cohort episodes crossed with regular
relative time bins. Event covariates are stored sparsely; a missing
`(rowId, timeId, covariateId)` row represents zero. The output contains
no absolute dates or source event identifiers.

## Usage

``` r
ds.omop.plan.person_period(
  plan,
  table,
  concept_set,
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

  Integer concept IDs.

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
