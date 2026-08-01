# Build the exact schema and semantic dependency manifest for a plan

The manifest is deliberately data-independent. It captures every source
table/column used implicitly by filters, cohort materialisation,
longitudinal grains, feature reductions, derived variables, vocabulary
expansion and output formatting. It is the contract bound by plan
harmonization.

## Usage

``` r
.plan_dependency_manifest(plan)
```

## Arguments

- plan:

  An `omop_plan`.

## Value

A list with `tables`, `needs_vocabulary_identity`, and semantic `issues`
that cannot be harmonized safely.
