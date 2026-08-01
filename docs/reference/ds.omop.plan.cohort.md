# Set a cohort filter on the plan

Attaches a cohort definition to the plan, restricting all downstream
outputs to the selected cohort episodes. A person may therefore
contribute multiple non-overlapping episodes under the same definition.
Exactly one of `cohort_definition_id` or `spec` must be provided. Use
`cohort_definition_id` to reference an existing cohort definition, or
`spec` to define a cohort inline using the DSL.

## Usage

``` r
ds.omop.plan.cohort(plan, cohort_definition_id = NULL, spec = NULL)
```

## Arguments

- plan:

  An `omop_plan` object.

- cohort_definition_id:

  Integer; ID of an existing cohort in the cohort table. Mutually
  exclusive with `spec`.

- spec:

  Named list; inline cohort specification DSL describing inclusion
  criteria. Mutually exclusive with `cohort_definition_id`.

## Value

The modified `omop_plan` with the cohort slot populated.

## See also

[`ds.omop.plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.md),
[`ds.omop.plan.cohort_membership`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.cohort_membership.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 42)

# Or with an inline spec
plan <- ds.omop.plan.cohort(plan, spec = list(
  sex = "Female", age_range = c(40, 65)
))
} # }
```
