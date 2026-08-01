# Create an output specification

Defines how to shape the extracted data into a result table. Each output
selects a subset of variables from the recipe, targets a population, and
specifies a layout type (e.g. wide person-level, long event-level, or
feature matrix). Outputs are passed to
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
via its `outputs` argument and determine the server-side plan structure
produced by
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md).

## Usage

``` r
omop_output(
  name = "output_1",
  type = c("wide", "long", "features", "survival", "intervals", "baseline",
    "temporal_covariates", "person_period"),
  variables = NULL,
  population_id = "base",
  options = list(),
  result_symbol = NULL
)
```

## Arguments

- name:

  Character; output table name (used as key in the recipe).

- type:

  Character; output layout type. One of `"wide"`, `"long"`,
  `"features"`, `"survival"`, `"intervals"`, `"baseline"`,
  `"temporal_covariates"`, or `"person_period"`. A `"long"` output that
  spans multiple source tables always splits into one per-table output
  (named `<name>_<table>`); there is no single cross-table joined frame.
  The former `"joined_long"` and `"covariates_sparse"` recipe labels are
  rejected because they had no faithful executable mapping; use split
  `"long"` outputs or the lower-level temporal-covariates plan API.

- variables:

  Character vector or `NULL`; variable names to include (`NULL` means
  all variables in the recipe).

- population_id:

  Character; which population to use (default `"base"`).

- options:

  Named list; type-specific options (e.g. `tar` for survival outputs).

- result_symbol:

  Character or `NULL`; R symbol name for the result on the server
  (auto-generated as `D_<name>` if `NULL`).

## Value

An `omop_output` object.

## See also

[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md),
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  variables = omop_variable(table = "condition_occurrence",
                            concept_id = 201820, format = "binary"),
  outputs = omop_output(name = "features_wide", type = "wide")
)
} # }
```
