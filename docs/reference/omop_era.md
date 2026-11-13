# Eras (intervals) built from an event

Recorded closed intervals are normalised by the era's own `end` and
`fallback` from the event's original recorded end and supply roles, then
coalesced per person and group: a new era starts when a record starts
more than `gap` unobserved days after the running maximum end. `by =
"ingredient"` groups drug concepts by their RxNorm ingredient (unmapped
concepts follow `unmapped`); `persistence` extends courses only;
`stockpile = TRUE` adds an effective-supply relation selected explicitly
by `covered_days(coverage = "effective_supply")`.

## Usage

``` r
omop_era(
  event,
  gap = 30L,
  persistence = 0L,
  min_days = 0L,
  by = c("event", "concept", "ingredient"),
  unmapped = c("own_concept", "drop"),
  end = c("recorded_end", "start"),
  fallback = c("days_supply", "none"),
  stockpile = FALSE,
  name = NULL
)
```

## Arguments

  - event:

    An `omop_event` (or its name).

  - gap:

    Tolerated uncovered days between records.

  - persistence:

    Days added to each course end.

  - min\_days:

    Minimum course length.

  - by:

    `"event"`, `"concept"` or `"ingredient"`.

  - unmapped:

    `"own_concept"` or `"drop"` (ingredient grouping).

  - end:

    `"recorded_end"` or `"start"` (point records).

  - fallback:

    `"days_supply"` or `"none"`.

  - stockpile:

    Logical.

  - name:

    Interval name.

## Value

An `omop_interval` object.
