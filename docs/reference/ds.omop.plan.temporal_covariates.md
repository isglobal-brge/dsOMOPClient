# Add a temporal (time-binned) covariates output to the plan

Produces FeatureExtraction-style sparse covariates binned into time
windows relative to the cohort index date. Returns four symbols on the
server: `<name>.temporalCovariates`, `<name>.covariateRef`,
`<name>.timeRef`, and `<name>.personRef`. The last maps cohort episodes
to pseudonymous persons. Requires a cohort to be set.

## Usage

``` r
ds.omop.plan.temporal_covariates(
  plan,
  table,
  concept_set = NULL,
  bin_width = 30L,
  window_start = -365L,
  window_end = 0L,
  analyses = c("binary"),
  name = "temporal"
)
```

## Arguments

- plan:

  An `omop_plan` object.

- table:

  Character; source OMOP table to extract covariates from.

- concept_set:

  Optional concept IDs or an OHDSI-style concept-set spec with
  `concepts`, `include_descendants`, `include_mapped`, and `exclude`.
  When `NULL`, all concepts present in the bounded event stream are
  retained, subject to the server concept cap.

- bin_width:

  Integer; width of each time bin in days.

- window_start:

  Integer; start of the observation window in days relative to the
  cohort index date (negative = before index).

- window_end:

  Integer; end of the observation window in days relative to the cohort
  index date (0 = index date).

- analyses:

  Character vector; types of analyses to compute. Supported values
  include `"binary"` and `"count"`.

- name:

  Character; output name used as a key in the plan's outputs list.

## Value

The modified `omop_plan` with the temporal covariates output appended.

## See also

[`ds.omop.plan.intervals`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.intervals.md),
[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
plan <- ds.omop.plan.temporal_covariates(plan,
  table = "condition_occurrence",
  concept_set = c(201826, 443238),
  bin_width = 30L,
  window_start = -365L,
  window_end = 0L,
  analyses = c("binary", "count")
)
} # }
```
