# Build a temporal filtering specification

Creates an `omop_temporal_spec` object that defines how events are
filtered relative to a cohort index date or calendar dates. The spec can
combine index-relative windows, calendar date ranges, and event
selection (first/last N) and deterministic minimum-gap collapsing.

## Usage

``` r
omop.temporal(
  index_window = NULL,
  calendar = NULL,
  event_select = NULL,
  min_gap = NULL
)
```

## Arguments

- index_window:

  Named list with `start` and `end` (integer days relative to the cohort
  index date). Negative values denote time before the index date.

- calendar:

  Named list with `start` and `end` (character ISO 8601 dates, e.g.
  `"2020-01-01"`).

- event_select:

  Named list with `order` (`"first"` or `"last"`), `n` (integer; number
  of events), and optional `by = "grain"` (default) or `by = "concept"`.
  Concept mode keeps the first/last N independently for each concept
  within a cohort episode when `index_window` is present, otherwise
  within each person.

- min_gap:

  Positive integer days, or a named list with `days`, optional
  `by = "concept"` or `"grain"`, and optional `keep = "first"` or
  `"last"`. Adjacent events no more than `days` apart form one chain.
  The normalized policy defaults to concept-specific chains represented
  by their first event.

## Value

An `omop_temporal_spec` object (a list with class
`c("omop_temporal_spec", "list")`).

## See also

[`ds.omop.plan.events`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.events.md),
[`omop.date_handling`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.date_handling.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Events within 1 year before each index episode; keep its first 3 events
temporal <- omop.temporal(
  index_window = list(start = -365, end = 0),
  event_select = list(order = "first", n = 3)
)
plan <- ds.omop.plan.events(plan, "conditions",
  "condition_occurrence", temporal = temporal)
} # }
```
