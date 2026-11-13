# Execute a recipe: compile to plan and run

Convenience function that compiles the recipe to an execution plan via
`recipe_to_plan` and immediately executes it via `ds.omop.plan.execute`.
Symbol names for server-side datasets are derived from the recipe's
output `result_symbol` fields, or auto-generated as `D_<name>`.

## Usage

``` r
recipe_execute(
  recipe,
  out = NULL,
  symbol = "omop",
  conns = NULL,
  output_mode = "memory",
  cohort = NULL,
  tables = NULL,
  combine = "union",
  on_small_site = c("error", "skip")
)
```

## Arguments

  - recipe:

    An `omop_recipe` object.

  - out:

    Named character vector; `output_name -> symbol_name` mapping. If
    `NULL`, auto-generates symbol names from the recipe's output
    specifications.

  - symbol:

    Character; OMOP session symbol on the server (default `"omop"`).

  - conns:

    DSI connections or `NULL` (uses active connections).

  - output\_mode:

    Character; `"memory"` (default) or `"staged"`. Passed through to
    `ds.omop.plan.execute`.

  - cohort:

    Optional recipe-level scope cohort applied at execution time: a
    `dsomop_cohort_handle`, a `cohort_definition_id`, or a server-side
    cohort table name. When supplied (with or without `tables`) it
    replaces any scope already on the recipe; `NULL` (the default)
    leaves the recipe's own scope untouched. Folded with `tables` by
    `combine` and intersected into every population (see `omop_recipe`).

  - tables:

    Optional character vector of `omop.table` symbol names to add to the
    execution-time scope (their distinct persons). May be combined with
    `cohort`.

  - combine:

    Character; how to fold the scope sources together: `"union"` (the
    default) or `"intersect"`.

  - on\_small\_site:

    Character; `"error"` (default) or `"skip"`, passed to
    `ds.omop.plan.execute`. Only insufficient final person support is
    skippable: skip keeps successful sites and returns `NULL` for sites
    whose publication fails final person support.

## Value

Invisibly, the output symbol mapping (a named character vector) for a
complete ordinary execution. When sites publish different outputs or
fresh names (the server allocates fresh names for occupied destinations;
a survival output publishes one symbol per TAR), a named per-site list
of the actual publication inventories; skipped sites have `NULL`
entries. As with `ds.omop.plan.execute`, the produced symbols are
recorded on the session so the manipulation wrappers can default to the
last one.

## See also

`recipe_to_plan`, `ds.omop.plan.execute`

## Examples

``` r
if (FALSE) { # \dontrun{
recipe_execute(recipe)
recipe_execute(recipe, output_mode = "staged")
# Or with explicit symbol mapping:
recipe_execute(recipe, out = c(features_wide = "D_features"))
# Scope every population to a cohort intersected with a workspace table:
recipe_execute(recipe, cohort = my_cohort, tables = "inclusion_set",
               combine = "intersect")
} # }
```
