# Reducers: how an event stream becomes one value per person or episode

The reducer contract (frozen, design section 2.7) fixes for every `fn`
its input, ordering, NULL handling, empty-window value and type. Rows
are ordered by `(start date, source, row key)`. A few rules worth
knowing: `first_value`/`last_value` skip NULL values by default
(`missing = "keep"` selects the row first); `count` and `binary` are 0
over an empty window, value reducers NA; `covered_days` measures the
union of the recorded intervals inside the window (never summed
durations; `observed_only = TRUE` intersects it with the observed days,
the PDC numerator); `supply_days` sums `days_supply` of fills starting
in the window (`fills = "clipped"` clips each fill to the window), the
MPR numerator; an undefined anchor gives NA for every reducer.

## Usage

``` r
omop_reduce(
  fn,
  value = NULL,
  window = NULL,
  which = NULL,
  n = 1L,
  side = c("either", "before", "after"),
  missing = c("skip", "keep"),
  distinct = NULL,
  p = NULL,
  agg = NULL,
  reference = NULL,
  predicate = NULL,
  reference_date = NULL,
  unit = NULL,
  coverage = c("recorded", "effective_supply"),
  fills = c("start_in_window", "clipped"),
  observed_only = FALSE
)
```

## Arguments

  - fn:

    Reducer name (see `fn` in the plan schema).

  - value:

    Value column (default: the normalised `value` for value reducers).

  - window:

    `NULL` or an `omop_window()`.

  - which:

    `NULL` or `"first"`, `"last"`, `"nth"`, `"closest"`.

  - n:

    Row number(s) for `nth_value` (a vector gives suffixed columns).

  - side:

    `closest_value`: `"either"`, `"before"`, `"after"`.

  - missing:

    `"skip"` (default) or `"keep"` NULL values before selection.

  - distinct:

    `count`: `"row"`, `"day"`, `"visit"`, `"concept"`.

  - p:

    Quantile probability in (0, 1\].

  - agg:

    `drug_duration`: `"mean"`, `"sum"`, `"max"`.

  - reference:

    Anchor for `days_since`/`days_until`.

  - predicate:

    `any`/`all`: an `omop_where()` predicate.

  - reference\_date:

    `time_since`: fixed ISO date (legacy).

  - unit:

    `time_since`: `"day"` or `"month"`.

  - coverage:

    `covered_days`: `"recorded"` or `"effective_supply"` (stockpiled
    eras only).

  - fills:

    `supply_days`: `"start_in_window"` or `"clipped"`.

  - observed\_only:

    `covered_days`: intersect with observed days.

## Value

An `omop_reducer` object.

## Examples

``` r
omop_reduce("closest_value", window = omop_window(-180, 0), side = "before")
omop_reduce("covered_days", window = omop_window(1, 90), observed_only = TRUE)
```
