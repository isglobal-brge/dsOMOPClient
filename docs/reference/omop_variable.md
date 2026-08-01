# Create a variable specification

Describes a single variable to extract from the CDM. Variables reference
a source table and column, and may include concept-level filtering and
output formatting options. Variables are the atomic building blocks
passed to
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
via its `variables` argument (or grouped with
[`omop_variable_block`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_block.md)
and passed via `blocks`).

## Usage

``` r
omop_variable(
  name = NULL,
  table,
  column = NULL,
  concept_id = NULL,
  concept_name = NULL,
  type = c("auto", "numeric", "categorical", "date", "boolean", "integer", "character"),
  format = c("raw", "binary", "count", "first_value", "last_value", "mean", "min", "max",
    "time_since", "binned", "age", "sex_mf", "obs_duration", "drug_duration", "sum",
    "n_distinct", "sd", "cv", "slope", "abnormal_high", "abnormal_low", "gap_max",
    "gap_mean", "duration_sum", "prior_obs", "followup", "demo_missingness", "charlson",
    "chads2", "chadsvasc", "dcsi", "hfrs"),
  value_source = NULL,
  time_window = NULL,
  suffix_mode = c("index", "range", "label"),
  filters = list(),
  visit_filter = NULL,
  concept_col = NULL,
  expand = FALSE,
  reference_date = NULL,
  unit = NULL
)
```

## Arguments

- name:

  Character; output column name (auto-generated from `concept_name`,
  `concept_id`, or `column` if `NULL`).

- table:

  Character; source OMOP CDM table (e.g. `"condition_occurrence"`).

- column:

  Character or `NULL`; source column to extract.

- concept_id:

  Integer or `NULL`; concept ID filter (for concept columns).

- concept_name:

  Character or `NULL`; human-readable concept name.

- type:

  Character; variable type hint. One of `"auto"`, `"numeric"`,
  `"categorical"`, `"date"`, `"boolean"`, `"integer"`, `"character"`.

- format:

  Character; output format. One of `"raw"`, `"binary"`, `"count"`,
  `"first_value"`, `"last_value"`, `"mean"`, `"min"`, `"max"`, and
  `"time_since"`, plus the documented derived and longitudinal summary
  formats. `"time_since"` requires a fixed `reference_date`;
  cohort-index recency remains episode-specific and is rejected.
  Calendar binning is configured with
  [`omop.date_handling`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.date_handling.md),
  not as a variable format.

- value_source:

  Character or `NULL`; column to extract value from (e.g.
  `"value_as_number"` for measurements).

- time_window:

  Named list with `start`/`end` offsets relative to index date, or
  `NULL` for no window constraint.

- suffix_mode:

  Character; how to name multi-column expansions (`"index"`, `"range"`,
  or `"label"`).

- filters:

  List of
  [`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md)
  objects to apply to this variable.

- visit_filter:

  Named list `list(concept_ids = ...)` or `NULL`; restrict this
  variable's events to visits of those `visit_concept_id` values (via
  the `visit_occurrence_id` link).

- concept_col:

  Character or `NULL`; override the concept column the
  `concept_id`/concept set scopes (default: the table's domain concept),
  e.g. `"unit_concept_id"` to extract a single unit for harmonization.

- expand:

  Logical; if `TRUE`, expand the concept to include vocabulary
  descendants server-side (default `FALSE`).

- reference_date:

  Character/Date or `NULL`; fixed ISO reference date required when
  `format = "time_since"`.

- unit:

  Character or `NULL`; `"day"` (default for `time_since`) or `"month"`.
  Months are complete calendar months, not fixed 30-day intervals.

## Value

An `omop_variable` object (a named list with class `"omop_variable"`).

## See also

[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md),
[`omop_variable_block`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_block.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  variables = omop_variable(
    table = "condition_occurrence",
    concept_id = 201820,
    concept_name = "Type 2 diabetes",
    format = "binary"
  ),
  outputs = omop_output(type = "wide")
)
} # }
```
