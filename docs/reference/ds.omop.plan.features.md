# Add feature extraction with feature specifications

Adds a feature-extraction output that computes person-level summary
columns (boolean, count, mean, etc.) from event-level data in a single
OMOP table. Each `omop_feature_spec` in `specs` produces one column in
the resulting data frame. Each spec is evaluated against its own concept
scope. No output-wide concept prefilter is added: such a prefilter would
change the meaning of an unscoped spec or of specs that use different
`concept_col` values.

## Usage

``` r
ds.omop.plan.features(
  plan,
  name,
  table,
  specs,
  grain = "person",
  temporal = NULL
)
```

## Arguments

- plan:

  An `omop_plan` object.

- name:

  Character; output name used as a key in the plan's outputs list.

- table:

  Character; source OMOP table name (e.g. `"condition_occurrence"`,
  `"measurement"`).

- specs:

  Named list of `omop_feature_spec` objects created by the
  `omop.feature.*` family of functions (e.g.
  [`omop.feature.boolean`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.boolean.md),
  [`omop.feature.count`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.count.md)).

- grain:

  Character; `"person"` (default) or `"episode"`. Episode grain
  preserves one row per cohort episode and requires
  `temporal$index_window`.

- temporal:

  Optional
  [`omop.temporal()`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.temporal.md)
  specification. Supply an `index_window` when `grain = "episode"` or
  when feature specs contain episode-relative `time_window` values.

## Value

The modified `omop_plan` with the features output appended.

## See also

[`omop.feature.boolean`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.boolean.md),
[`omop.feature.count`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.count.md),
[`ds.omop.plan.events`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.events.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.features(plan,
  name = "lab_features",
  table = "measurement",
  specs = list(
    has_glucose = omop.feature.boolean(c(3004410)),
    glucose_mean = omop.feature.mean_value(c(3004410))
  )
)
} # }
```
