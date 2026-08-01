# Print method for extraction plans

Displays a human-readable summary of an `omop_plan` including the cohort
definition, all configured outputs with their types and key parameters,
and plan-wide options.

## Usage

``` r
# S3 method for class 'omop_plan'
print(x, ...)
```

## Arguments

- x:

  An `omop_plan` object.

- ...:

  Additional arguments (ignored).

## Value

Invisible `x`, for use in pipelines.

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.baseline(plan)
print(plan)
} # }
```
