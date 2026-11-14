# A v1 refusal of a shape the 2.10.3 language permits

The v1 compiler refuses some 2.10.1-style recipes that 2.10.3 newly
permits (design 5, corpus class `newly_permitted`): NOT filter groups,
the `covariates_sparse` label (B-22), feature outputs mixing windowed
and unwindowed variables (B-19), survival outcomes and interval or
temporal-covariate sources narrowed by visit or concept column, interval
outputs mixing concept-scoped and unscoped variables of a table, and
temporal covariates declaring different analyses per concept. Those
refusals carry this condition class; execution then routes the recipe to
its language-2 plan. Every other v1 refusal stands.

## Usage

``` r
.v1_newly_permitted(...)
```

## Arguments

  - ...:

    Message parts.

## Value

A condition object.
