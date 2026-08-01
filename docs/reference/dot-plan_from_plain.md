# Reconstruct an omop_plan from its plain representation

Inverse of
[`.plan_plain`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-plan_plain.md):
applies
[`.plan_restore`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-plan_restore.md)
to recover atomic vectors and integer types, then re-stamps the
`omop_plan` class so
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)
accepts the result unchanged.

## Usage

``` r
.plan_from_plain(data)
```

## Arguments

- data:

  A parsed plain plan (from JSON or YAML).

## Value

An `omop_plan` object.
