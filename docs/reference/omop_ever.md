# Simple-layer variables (shortcuts)

Each shortcut returns an ordinary language-2 variable
(`omop_variable(event = omop_event(table, concepts, ...), reducer =
omop_reduce(...))`); its name comes from the list it is placed in
(`baseline = list(hba1c = omop_value_at_index(...))`). The windows are
relative to the index date: `before = n` is `omop_window(-n, 0)`, `after
= n` is `omop_window(1, n)`, `window` is any `omop_window`; the three
are mutually exclusive, and without any of them the window is any time
up to and including the index date (`omop_window(NULL, 0)`). `Inf`
leaves the far side unbounded.

## Usage

``` r
omop_ever(
  table,
  concepts = NULL,
  before = NULL,
  after = NULL,
  window = NULL,
  ...
)

omop_count(
  table,
  concepts = NULL,
  before = NULL,
  after = NULL,
  window = NULL,
  distinct = "row",
  ...
)

omop_first_value(
  table,
  concepts,
  value = NULL,
  unit = NULL,
  before = NULL,
  after = NULL,
  window = NULL
)

omop_last_value(
  table,
  concepts,
  value = NULL,
  unit = NULL,
  before = NULL,
  after = NULL,
  window = NULL
)

omop_mean_value(
  table,
  concepts,
  value = NULL,
  unit = NULL,
  before = NULL,
  after = NULL,
  window = NULL
)

omop_min_value(
  table,
  concepts,
  value = NULL,
  unit = NULL,
  before = NULL,
  after = NULL,
  window = NULL
)

omop_max_value(
  table,
  concepts,
  value = NULL,
  unit = NULL,
  before = NULL,
  after = NULL,
  window = NULL
)

omop_value_at_index(table, concepts, tolerance = c(-90, 30), unit = NULL)

omop_days_since(table, concepts)

omop_days_until(table, concepts)

omop_first_date(table, concepts)

omop_last_date(table, concepts)
```

## Arguments

  - table:

    OMOP table name, or an `omop_event` / `omop_era` (then `concepts`,
    `unit`, `value` and `...` are not used).

  - concepts:

    Concept IDs, an `omop_concept_set`, the name of a registry concept
    set, or `NULL` for every row of the table.

  - before, after:

    Days before (up to and including the index date) or after (from the
    day after the index date); `Inf` for no limit.

  - window:

    An `omop_window`.

  - ...:

    Further `omop_event` arguments (`descendants`, `mapped`, `exclude`,
    `where`, `visit`, `value`, `unit`, `concept_col`, `end`, `name`,
    `on_missing_table`).

  - distinct:

    What `omop_count()` counts: `"row"`, `"day"`, `"visit"` or
    `"concept"`.

  - value:

    Numeric value column of the event (default: the table's value role,
    `value_as_number`).

  - unit:

    A unit concept ID (restriction) or an `omop_unit` (conversion).

  - tolerance:

    Window `c(start, end)` around the index date.

## Value

An `omop_variable`.

## Details

  - `omop_ever()`: 1 if the event occurs in the window, else 0 (reducer
    `binary`).

  - `omop_count()`: number of rows (or distinct days, visits, concepts)
    in the window.

  - `omop_first_value()`, `omop_last_value()`, `omop_mean_value()`,
    `omop_min_value()`, `omop_max_value()`: the normalised value of the
    first / last row in the window, or its mean, minimum or maximum.

  - `omop_value_at_index()`: the value closest to the index date within
    `tolerance` days (reducer `closest_value`).

  - `omop_days_since()` / `omop_days_until()`: days from the last row on
    or before / the first row on or after the index date.

  - `omop_first_date()` / `omop_last_date()`: day of the first / last
    row at any time, relative to the index date.

A numeric `unit = U` restricts the event to rows recorded in unit `U`
(`unit_concept_id == U`); it never converts. Give an `omop_unit` to
convert values to one unit.

## See also

`omop_study`, `omop_reduce`

## Examples

``` r
omop_ever("condition_occurrence", 201826, descendants = TRUE, before = 365)
omop_count("visit_occurrence", 9201, before = 365)
omop_value_at_index("measurement", 3004410, unit = 8554)
```
