# Add an event-level extraction to the plan

Extracts rows from a single OMOP clinical data table, optionally
filtered by concept set, time window, temporal specification, and custom
filters. The output format is controlled by the `representation`
parameter (long, wide, or features).

## Usage

``` r
ds.omop.plan.events(
  plan,
  name,
  table,
  columns = NULL,
  concept_set = NULL,
  time_window = NULL,
  temporal = NULL,
  date_handling = NULL,
  filters = NULL,
  visit_filter = NULL,
  concept_col = NULL,
  representation = list(format = "long")
)
```

## Arguments

- plan:

  An `omop_plan` object.

- name:

  Character; output name used as a key in the plan's outputs list.

- table:

  Character; source OMOP table name (e.g. `"condition_occurrence"`,
  `"drug_exposure"`).

- columns:

  Character vector; columns to include from the table. If `NULL`, the
  server selects default columns.

- concept_set:

  Numeric vector or concept set spec; concept IDs used to filter rows
  via the standard concept ID column of the table.

- time_window:

  Named list with `start_date` and `end_date` for calendar-based
  filtering.

- temporal:

  An `omop_temporal_spec` object or list; temporal filtering relative to
  a cohort index date. See
  [`omop.temporal`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.temporal.md).

- date_handling:

  A list; date handling specification controlling how date columns are
  transformed. See
  [`omop.date_handling`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.date_handling.md).

- filters:

  Named list; additional custom filter DSL expressions (nested
  `and`/`or` of leaves, each `list(var=, op=, value=)`). Validated
  fail-closed server-side: leaves on identifier or blocked columns, and
  narrow fingerprinting operators, are rejected. Use this to filter by
  `unit_concept_id` or a `*_type_concept_id` for unit/type scoping.

- visit_filter:

  Named list `list(concept_ids = ...)`; restrict events to visits of
  those `visit_concept_id` values via the `visit_occurrence_id` link.

- concept_col:

  Character; override the concept column the `concept_set` scopes
  (default: the table's domain concept), e.g. `"unit_concept_id"` to
  extract one unit for harmonization.

- representation:

  Named list with `format` (one of `"long"`, `"wide"`, `"features"`, or
  `"sparse"`) and optional format-specific settings.
  Wide/features/sparse may set `grain` to `"person"` (default) or
  `"episode"`; an index-relative window requires episode grain.

## Value

The modified `omop_plan` with the event-level output appended.

## See also

[`omop.temporal`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.temporal.md),
[`omop.date_handling`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.date_handling.md),
[`ds.omop.plan.features`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.features.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.events(plan,
  name = "conditions",
  table = "condition_occurrence",
  concept_set = c(201826, 443238),
  temporal = omop.temporal(index_window = list(start = -365, end = 0)),
  date_handling = omop.date_handling(mode = "relative")
)
} # }
```
