# Lint a recipe for common authoring mistakes (pure client-side)

Walks an `omop_recipe` and returns a tidy report of problems without
contacting any server. The first and most important check is whether the
recipe even compiles:
[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md)
is run inside `tryCatch` and any compile error becomes the `no_compile`
lint. Remaining rules inspect outputs, variables, and population filters
for structural and disclosure-safety issues that would otherwise surface
only at execution time on the server.

## Usage

``` r
recipe_lint(recipe)
```

## Arguments

- recipe:

  An `omop_recipe` object, or `NULL`.

## Value

A `data.frame` with columns `severity`, `code`, `message`, `locus` (zero
rows if the recipe is clean).

## Details

Severities are `"ERROR"` (will fail), `"WARNING"` (likely rejected or
wrong), and `"INFO"` (advisory). Rules:

- no_compile (ERROR):

  [`recipe_to_plan()`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md)
  throws.

- empty_output (ERROR):

  an output resolves to zero variables.

- dup_output (ERROR):

  duplicate output names.

- concept_zero (ERROR):

  a non-derived variable has an unmapped `concept_id` (0 or NA).

- concept_unnamed (WARNING):

  `concept_id` present but `concept_name` missing.

- time_since_spec (ERROR):

  a `time_since` variable lacks a valid fixed ISO
  `derived$reference_date` or `derived$unit`.

- age_no_index (WARNING):

  `format = "age"` with index reference but the base population has
  neither a cohort nor a date filter.

- narrow_filter (WARNING):

  population `age_range` width \< 5 years (server rejects).
  Calendar-window width is server policy and is therefore not hard-coded
  by this client lint.

- highcard_factor (WARNING):

  a raw `_concept_id` column with `factor_concepts = TRUE`, or a
  categorical format on a high-cardinality domain.

- window_inverted (WARNING):

  a variable `time_window` with `start > end`.

- long_split (INFO):

  a `"long"` output spanning \> 1 table.

- no_value_source (INFO):

  an aggregate format with no `value_source`.

Edge cases: a `NULL` recipe returns a single `empty_recipe` info row; a
recipe with no outputs adds a `no_output` warning.

## See also

[`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md),
[`recipe_validate`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_validate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(outputs = omop_output(type = "wide"))
recipe_lint(recipe)
} # }
```
