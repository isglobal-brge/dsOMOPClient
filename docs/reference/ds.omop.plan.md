# Create a new extraction plan

Initialises an empty `omop_plan` object that serves as the container for
cohort definitions, output specifications, and plan-wide options. Build
up the plan by piping it through `ds.omop.plan.*` helpers such as
[`ds.omop.plan.cohort`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.cohort.md),
[`ds.omop.plan.baseline`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.baseline.md),
and
[`ds.omop.plan.events`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.events.md).

## Usage

``` r
ds.omop.plan()
```

## Value

An `omop_plan` object (a list with class `c("omop_plan", "list")`)
containing empty slots for cohort, anchor, outputs, and options.

## See also

[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md),
[`print.omop_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/print.omop_plan.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
plan <- ds.omop.plan.baseline(plan)
} # }
```
