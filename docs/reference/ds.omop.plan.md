# Create a new extraction plan

Initialises an empty `omop_plan` object that serves as the container for
cohort definitions, output specifications, and plan-wide options. Build
up the plan by piping it through `ds.omop.plan.*` helpers such as
`ds.omop.plan.cohort`, `ds.omop.plan.baseline`, and
`ds.omop.plan.events`.

## Usage

``` r
ds.omop.plan()
```

## Value

An `omop_plan` object (a list with class `c("omop_plan", "list")`)
containing empty slots for cohort, anchor, outputs, and options.

## See also

`ds.omop.plan.execute`, `print.omop_plan`

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
plan <- ds.omop.plan.baseline(plan)
} # }
```
