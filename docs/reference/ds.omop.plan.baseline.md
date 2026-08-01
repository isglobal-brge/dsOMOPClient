# Add a baseline demographics output to the plan

Produces one row per cohort episode, preserving `cohort_row_id` when a
person has recurrent membership, with demographics from the person table
and optional derived fields. Requires a cohort to be set. This is the
recommended way to retrieve cohort-indexed demographic variables because
it can compute episode-relative fields such as age at index.

## Usage

``` r
ds.omop.plan.baseline(
  plan,
  columns = c("gender_concept_id", "race_concept_id"),
  derived = c("age_at_index"),
  name = "baseline"
)
```

## Arguments

- plan:

  An `omop_plan` object.

- columns:

  Character vector; person-table columns to include (e.g.
  `"gender_concept_id"`, `"race_concept_id"`). Exact birth components
  are not releasable; request `"age_at_index"` through `derived`
  instead. Pass a *named* vector to rename columns. Unnamed entries keep
  their source name. Identifier columns cannot be renamed (they are
  stripped server-side regardless).

- derived:

  Character vector; derived fields to compute. Supported values include
  `"age_at_index"`, `"prior_observation"`, and `"future_observation"`.

- name:

  Character; output name used as a key in the plan's outputs list and as
  the default symbol name on the server.

## Value

The modified `omop_plan` with the baseline output appended.

## See also

[`ds.omop.plan.person_level`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.person_level.md),
[`ds.omop.plan.cohort`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.cohort.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
plan <- ds.omop.plan.baseline(plan,
  columns = c("gender_concept_id", "race_concept_id"),
  derived = c("age_at_index", "prior_observation")
)
} # }
```
