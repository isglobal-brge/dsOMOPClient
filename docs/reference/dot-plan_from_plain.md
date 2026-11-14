# Reconstruct an omop\_plan from its plain representation

Inverse of `.plan_plain`: applies `.plan_restore` to recover atomic
vectors and integer types, then re-stamps the `omop_plan` class so
`ds.omop.plan.execute` accepts the result unchanged.

## Usage

``` r
.plan_from_plain(data)
```

## Arguments

  - data:

    A parsed plain plan (from JSON or YAML).

## Value

An `omop_plan` object.
