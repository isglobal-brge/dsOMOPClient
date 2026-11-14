# Windows, bounds and anchors

`omop_window(start, end, anchor)` is an inclusive range of whole days
relative to an anchor (default the index date). `NULL` (or `-Inf` for
`start`, `Inf` for `end`) leaves an endpoint unbounded. Each endpoint
may instead be an `omop_bound(anchor, offset)` with its own anchor, so
start and end can be anchored independently (e.g. on-treatment follow-up
ending 30 days after the treatment course).

## Usage

``` r
omop_window(start = NULL, end = NULL, anchor = "index")

omop_bound(anchor, offset = 0L)

omop_anchor(
  event,
  which = c("first", "last", "nth", "latest_end"),
  n = 1L,
  field = c("start", "end"),
  scope = c("person", "episode"),
  search = NULL
)
```

## Arguments

  - start, end:

    Day offsets, `NULL`/`-Inf`/`Inf`, or `omop_bound()`.

  - anchor:

    Anchor of integer offsets (default `"index"`).

  - offset:

    Integer day offset of a bound.

  - event:

    An `omop_event`, `omop_era` or registry name.

  - which:

    `"first"`, `"last"`, `"nth"` or `"latest_end"`.

  - n:

    Row number for `which = "nth"`.

  - field:

    `"start"` or `"end"` of the selected row.

  - scope:

    `"person"` (default, may precede the index) or `"episode"`.

  - search:

    `NULL` or an `omop_window()` restricting candidates.

## Value

`omop_window`, `omop_bound` or `omop_anchor` objects.

## Details

Anchors are `"index"`, `"index_end"`, `"observation_start"`,
`"observation_end"`, an ISO date, or `omop_anchor(event, ...)`: one
complete event row selected by the order `(start_date, source, key)`;
`field = "end"` reads that row's end; `latest_end` selects by end (only
with `field = "end"`); `scope = "episode"` confines the search to the
episode's observed span; `search` is a window restricting the candidate
rows. A missing anchor makes a window undefined (criteria UNKNOWN,
reducers NA).

## Examples

``` r
omop_window(-365, -1)
omop_window(omop_bound("index", 1), omop_bound("index_end", 30))
```
