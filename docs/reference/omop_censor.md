# Censoring by an event or era

A censoring source for survival outputs: the date selected relative to
the effective entry (`first_end_after_entry` by default, everywhere)
plus `offset` days, optionally restricted to a `search` window. `missing
= "ineligible"` makes an episode without such a date ineligible.
`when_index_source` (amendment A-7) restricts the censor to episodes
whose `index_source` is one of the given arm tags, so a two-arm study
can censor at a switch to the *other* arm only.

## Usage

``` r
omop_censor(
  source,
  which = c("first_end_after_entry", "first_start_after_entry", "containing_entry_end"),
  offset = 0L,
  search = NULL,
  missing = c("ignore", "ineligible"),
  when_index_source = NULL
)
```

## Arguments

  - source:

    An event, era or registry name.

  - which:

    `"first_end_after_entry"`, `"first_start_after_entry"` or
    `"containing_entry_end"`.

  - offset:

    Integer days added (e.g. a grace period).

  - search:

    `NULL` or an `omop_window()`.

  - missing:

    `"ignore"` or `"ineligible"`.

  - when\_index\_source:

    `NULL` or a non-empty unique character vector of index-source tags
    (arm names of the consumer's index).

## Value

An `omop_censor` object.
