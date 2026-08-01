# Add a cohort membership output to the plan

Produces a protected cohort-membership view with `row_id` (the
`cohort_row_id` alias), pseudonymized `subject_id`,
`cohort_definition_id`, and dates transformed by the configured date
policy (removed by default). Recurrent episodes remain distinct.
Requires a cohort to be set on the plan via
[`ds.omop.plan.cohort`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.cohort.md);
this is not an unrestricted raw export of the OHDSI cohort table.

## Usage

``` r
ds.omop.plan.cohort_membership(plan, name = "cohort_membership")
```

## Arguments

- plan:

  An `omop_plan` object.

- name:

  Character; output name used as a key in the plan's outputs list.

## Value

The modified `omop_plan` with the cohort membership output appended.

## See also

[`ds.omop.plan.cohort`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.cohort.md),
[`ds.omop.plan.baseline`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.baseline.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
plan <- ds.omop.plan.cohort_membership(plan, name = "my_cohort")
} # }
```
