# Resolve the population a (possibly table-split) plan output belongs to

A "long"/features output spanning multiple tables is split into
`<name>_<table>` children in the plan. To recover an output's
`population_id` we match the plan output name to a recipe output: a
direct hit wins; otherwise the longest recipe-output name it is prefixed
by (`<recipe_name>_...`) is its parent. Falls back to `"base"` when no
recipe output matches (defensive; should not happen for plan-built
outputs).

## Usage

``` r
.recipe_output_population(plan_out_name, recipe_outputs, recipe_out_names)
```

## Arguments

- plan_out_name:

  Character; a name in `plan$outputs`.

- recipe_outputs:

  Named list of `omop_output` objects.

- recipe_out_names:

  Character vector; `names(recipe_outputs)`.

## Value

Character; the target population ID.
