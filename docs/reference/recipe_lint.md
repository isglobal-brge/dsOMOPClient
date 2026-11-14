# Lint a recipe for common authoring mistakes (pure client-side)

Walks an `omop_recipe` and returns a tidy report of problems without
contacting any server. The first and most important check is whether the
recipe even compiles: `recipe_to_plan` is run inside `tryCatch` and any
compile error becomes the `no_compile` lint. Remaining rules inspect
outputs, variables, and population filters for structural and
disclosure-safety issues that would otherwise surface only at execution
time on the server.

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

  - no\_compile (ERROR):

    `recipe_to_plan()` throws.

  - empty\_output (ERROR):

    an output resolves to zero variables.

  - dup\_output (ERROR):

    duplicate output names.

  - concept\_zero (ERROR):

    a non-derived variable has an unmapped `concept_id` (0 or NA).

  - concept\_unnamed (WARNING):

    `concept_id` present but `concept_name` missing.

  - time\_since\_spec (ERROR):

    a `time_since` variable lacks a valid fixed ISO
    `derived$reference_date` or `derived$unit`.

  - age\_no\_index (WARNING):

    `format = "age"` with index reference but the base population has
    neither a cohort nor a date filter.

  - highcard\_factor (WARNING):

    a raw `_concept_id` column with `factor_concepts = TRUE`, or a
    categorical format on a high-cardinality domain.

  - window\_inverted (WARNING):

    a variable `time_window` with `start > end`.

  - long\_split (INFO):

    a `"long"` output spanning \> 1 table.

  - no\_value\_source (INFO):

    an aggregate format with no `value_source`.

  - tar\_default, censoring\_default, age\_group\_default, coverage,
    collapse, representation, missing\_skip (INFO):

    a default the recipe relies on, stated (e.g. "follow-up ends 730
    days after index").

  - quasi\_identifier (INFO):

    a permitted birth component, age, age band, birth-year band or date
    column, one row per variable; final publication support and the
    absolute-date policy still apply. Aggregate admission is unchanged.

  - measurement\_pair (INFO):

    `has_measurement` / `missing_measurement` are not complements.

  - parent\_inheritance (INFO):

    a `parent_id` now restricts the child population.

  - language\_2\_only (INFO):

    a recipe of 2.10.1 constructors whose shape 2.10.3 newly permits
    (e.g. a NOT group) has no v1 plan and runs as its language-2 plan on
    dsOMOP 2.10.3 servers only.

The lint has no mirror of server disclosure gates (no width floors, no
mandatory bins): from 2.10.3 the server's final publication rule is the
only release criterion on assign paths. Edge cases: a `NULL` recipe
returns a single `empty_recipe` info row; a recipe with no outputs adds
a `no_output` warning.

## See also

`recipe_to_plan`, `recipe_validate`

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(outputs = omop_output(type = "wide"))
recipe_lint(recipe)
} # }
```
