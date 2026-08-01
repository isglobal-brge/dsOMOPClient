# Build a date handling specification

Creates a specification controlling how date columns are transformed in
event-level outputs. Dates can be kept as-is, converted to relative days
from an index date, binned into calendar periods, or removed entirely
for privacy.

## Usage

``` r
omop.date_handling(
  mode = "remove",
  reference = "index",
  bin_width = NULL,
  date_columns = NULL
)
```

## Arguments

- mode:

  Character; transformation mode. Defaults to `"remove"`
  (privacy-preserving). One of `"absolute"` (keep original dates),
  `"relative"` (convert to days from reference), `"binned"` (aggregate
  into calendar bins), or `"remove"` (drop all date columns).

- reference:

  Character; reference point for relative mode. Currently only `"index"`
  (cohort index date) is supported.

- bin_width:

  Character; bin granularity for binned mode. One of `"week"`,
  `"month"`, or `"year"`.

- date_columns:

  Character vector; specific date columns to transform. If `NULL`, all
  date columns in the output are transformed.

## Value

A list with elements `mode`, `reference`, `bin_width`, and
`date_columns`.

## See also

[`ds.omop.plan.events`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.events.md),
[`omop.temporal`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.temporal.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert dates to days relative to cohort index
dh <- omop.date_handling(mode = "relative", reference = "index")

# Bin dates by month, remove exact dates
dh <- omop.date_handling(mode = "binned", bin_width = "month")

plan <- ds.omop.plan.events(plan, "conditions",
  "condition_occurrence", date_handling = dh)
} # }
```
