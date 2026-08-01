# Portable plain-list representation of a plan

Produces the version-tagged, class-free list that
[`ds.omop.plan.save`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.save.md)
serializes. Preserves every field an `omop_plan` carries (`cohort`,
`anchor`, `outputs` with their nested `filters$custom` and/or trees,
`concept_set`, `time_window`, representation `format`s, and `options`).

## Usage

``` r
.plan_plain(plan)
```

## Arguments

- plan:

  An `omop_plan` object.

## Value

A plain list with a `version` tag and the plan fields.
