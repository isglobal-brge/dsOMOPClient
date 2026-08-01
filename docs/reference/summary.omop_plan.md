# Summarise an extraction plan

Produces a compact, human-readable overview of an `omop_plan`: the
cohort it targets, one row per configured output (type, name, and key
parameters), and the plan-wide disclosure options. This is the headless
equivalent of inspecting a plan interactively.

## Usage

``` r
# S3 method for class 'omop_plan'
summary(object, ...)
```

## Arguments

- object:

  An `omop_plan` object.

- ...:

  Additional arguments (ignored).

## Value

Invisibly, a data frame with one row per output (columns `type`, `name`,
`detail`); printed as a formatted summary as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.baseline(plan)
summary(plan)
} # }
```
