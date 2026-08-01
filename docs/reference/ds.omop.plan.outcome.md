# Add an outcome extraction (convenience wrapper)

Convenience function that wraps
[`ds.omop.plan.events`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.events.md)
with `representation = list(format = "features")` to produce a
person-level binary outcome indicator for the given concept set. This is
a shorthand for defining an event-level features output focused on
outcome identification.

## Usage

``` r
ds.omop.plan.outcome(plan, name, concept_set, table = "condition_occurrence")
```

## Arguments

- plan:

  An `omop_plan` object.

- name:

  Character; output name used as a key in the plan's outputs list.

- concept_set:

  Numeric vector; concept IDs that define the outcome event.

- table:

  Character; source OMOP table containing the outcome events.

## Value

The modified `omop_plan` with the outcome output appended.

## See also

[`ds.omop.plan.events`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.events.md),
[`ds.omop.plan.survival`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.survival.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.outcome(plan,
  name = "diabetes_outcome",
  concept_set = c(201826),
  table = "condition_occurrence"
)
} # }
```
