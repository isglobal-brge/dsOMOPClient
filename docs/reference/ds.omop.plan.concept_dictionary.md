# Add a concept dictionary output to the plan

Scans other outputs in the plan for concept IDs and produces a lookup
table with concept names, domains, vocabulary IDs, and which outputs
reference each concept. Useful for translating numeric concept IDs in
other output tables into human-readable labels.

## Usage

``` r
ds.omop.plan.concept_dictionary(
  plan,
  source_outputs = NULL,
  name = "concept_dictionary"
)
```

## Arguments

- plan:

  An `omop_plan` object.

- source_outputs:

  Character vector; names of outputs to scan for concept IDs. If `NULL`
  (the default), all non-dictionary outputs in the plan are scanned.

- name:

  Character; output name used as a key in the plan's outputs list.

## Value

The modified `omop_plan` with the concept dictionary output appended.

## See also

[`ds.omop.plan.options`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.options.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.events(plan, "conditions", "condition_occurrence",
  concept_set = c(201826))
plan <- ds.omop.plan.concept_dictionary(plan)
} # }
```
