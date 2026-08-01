# Strip S3 classes from a plan for clean serialization

Recursively drops every S3 class (e.g. `omop_plan`, `omop_feature_spec`,
`omop_temporal_spec`) so the structure serializes to plain JSON/YAML
mappings and arrays. The data itself is untouched. Mirrors
`.recipe_strip_classes`.

## Usage

``` r
.plan_strip_classes(x)
```

## Arguments

- x:

  Any object.

## Value

`x` with nested lists reduced to plain lists.
