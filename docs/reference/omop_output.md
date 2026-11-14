# Create an output specification

Defines how to shape the extracted data into a result table. Each output
selects a subset of variables from the recipe, targets a population, and
specifies a layout type (e.g. wide person-level, long event-level, or
feature matrix). Outputs are passed to `omop_recipe` via its `outputs`
argument and determine the server-side plan structure produced by
`recipe_to_plan`.

## Usage

``` r
omop_output(
  name = "output_1",
  type = NULL,
  variables = NULL,
  population_id = "base",
  options = list(),
  result_symbol = NULL,
  rows = NULL,
  grain = NULL,
  columns = NULL,
  format = NULL,
  sources = NULL,
  collapse = NULL
)
```

## Arguments

  - name:

    Character; output table name (used as key in the recipe).

  - type:

    Character; output layout type. One of `"wide"`, `"long"`,
    `"features"`, `"survival"`, `"intervals"`, `"baseline"`,
    `"temporal_covariates"`, or `"person_period"`. A `"long"` output
    that spans multiple source tables always splits into one per-table
    output (named `<name>_<table>`); there is no single cross-table
    joined frame. The former `"joined_long"` label is rejected because
    it had no faithful executable mapping; use split `"long"` outputs.
    The `"covariates_sparse"` label (2.10.3, B-22) is the language-2
    `format = "sparse"` over the output's variables as sources; it has
    no v1 rendering and runs on dsOMOP 2.10.3 servers.

  - variables:

    Character vector or `NULL`; variable names to include (`NULL` means
    all variables in the recipe).

  - population\_id:

    Character; which population to use (default `"base"`).

  - options:

    Named list; type-specific options (e.g. `tar` for survival outputs).

  - result\_symbol:

    Character or `NULL`; R symbol name for the result on the server
    (auto-generated as `D_<name>` if `NULL`).

  - rows:

    (2.10.3) Alias of `population_id` (give one).

  - grain:

    (2.10.3) `"episode"` or `"person"`; default episode over an indexed
    population, person otherwise. Person grain over an indexed
    population needs `collapse`.

  - columns:

    (2.10.3) Alias of `variables` (give one).

  - format:

    (2.10.3) Language-2 format: `"wide"`, `"long"`, `"survival"`,
    `"intervals"`, `"temporal_covariates"`, `"person_period"`,
    `"sparse"`, `"membership"`, `"dictionary"` or `"joined"`. `type`
    (the legacy presets) and `format` are alternatives; giving neither
    keeps the legacy `type = "wide"`.

  - sources:

    (2.10.3) Event/era sources of a `long` output.

  - collapse:

    (2.10.3) `"first_episode"`, `"last_episode"` or `"any"`: person
    grain over an indexed population.

## Value

An `omop_output` object.

## Details

The `baseline` preset reports age as the banded legacy `age_group` (the
server's default grid), while a `wide` output with `omop_age()` reports
exact annual age; `omop_age(bands = TRUE/FALSE, age_breaks =)` chooses
explicitly in every format, and `recipe_explain()` states the choice.

Survival outputs keep the legacy 730-day time-at-risk default when no
`tar`/`tars` is given (`print()`, `recipe_explain()` and `recipe_lint()`
state it); give `tar = omop_window(1, NULL)` for unbounded follow-up. A
language-2 survival output needs an explicit `censor` list: only listed
sources cap follow-up.

## See also

`omop_recipe`, `recipe_to_plan`

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
