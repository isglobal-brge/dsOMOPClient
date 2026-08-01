# Set the recipe-level population scope

Records a recipe-wide scope that the server folds into ONE cohort and
intersects into every population before extraction. The scope mixes an
optional cohort reference (a `dsomop_cohort_handle`, a
`cohort_definition_id`, or a server-side cohort table name) with zero or
more workspace `omop.table` symbol *names*; the sources are combined on
the person key by `combine` (`"union"`/`"intersect"`). The cohort
reference is normalised with the shared
[`.cohort_scope_arg`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-cohort_scope_arg.md)
resolver (the same one the exploration / analysis wrappers use) so the
server receives the value its `.resolveCohortArg` expects; table symbol
names travel as strings for the server to resolve to frames (matching
the `ds.omop.analysis.run` scope contract). Stored on `recipe$scope` and
serialized to `plan$scope` by
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md).
With no cohort and no tables the scope is cleared
(`recipe$scope <- NULL`), so a plain recipe carries no scope and is
byte-identical to one built without it.

## Usage

``` r
recipe_set_scope(recipe, cohort = NULL, tables = NULL, combine = "union")
```

## Arguments

- recipe:

  An `omop_recipe` object.

- cohort:

  Cohort reference or `NULL`.

- tables:

  Character vector of `omop.table` symbol names, or `NULL`.

- combine:

  Character; `"union"` (default) or `"intersect"`.

## Value

The modified `omop_recipe` object.

## See also

[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md),
[`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md),
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md),
[`.cohort_scope_arg`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-cohort_scope_arg.md)
