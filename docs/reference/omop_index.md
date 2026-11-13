# Index definitions, episode ends and observation coverage

`omop_index()` defines the episodes (index dates) of an indexed
population from an event, an era, a results cohort
(`list(cohort_definition_id = n)`) or `omop_any_of(...)` arms. `select =
"first"`, `"last"` and `"nth"` select per person *before* the criteria
are evaluated (the legacy, Circe primary-limit order);
`"first_eligible"` keeps the earliest candidate whose complete criterion
tree is TRUE; `"all"` keeps every candidate. `offset` shifts the index
date before coverage lookup; end-before-start episodes are privately
ineligible.

## Usage

``` r
omop_index(
  source,
  select = c("first", "first_eligible", "last", "all", "nth"),
  n = 1L,
  offset = 0L,
  end = omop_end("observation_end"),
  restrict_to_observation = TRUE
)

omop_any_of(..., order = NULL)

omop_end(
  strategy = c("observation_end", "fixed", "event_end", "interval_end"),
  days = NULL,
  pad = 0L
)

omop_coverage(
  policy = c("unique", "earliest", "union"),
  on_ambiguous = c("drop", "error"),
  on_missing = c("drop", "keep"),
  gap = 0L
)
```

## Arguments

  - source:

    An event, era, `list(cohort_definition_id = n)` or `omop_any_of()`.

  - select:

    Candidate selection (see Details).

  - n:

    Row for `select = "nth"`.

  - offset:

    Day offset added to the source start.

  - end:

    An `omop_end()`.

  - restrict\_to\_observation:

    Logical; `FALSE` keeps uncovered indexes (coverage `on_missing =
    "keep"`).

  - ...:

    Index sources (events or eras) for `omop_any_of()`.

  - order:

    Optional character vector naming the arms in their declared
    tie-break order (default: argument order).

  - strategy:

    End strategy.

  - days:

    Days after the index for `strategy = "fixed"`.

  - pad:

    Non-negative days added to an event/interval end.

  - policy, on\_ambiguous, on\_missing, gap:

    Coverage policy.

## Value

`omop_index`, `omop_any_of`, `omop_end` or `omop_coverage` objects.

## Details

`omop_end()` sets the episode end (`index_end`): `observation_end`
(default), `fixed` (`days` after the index), `event_end` (the source
row's end plus `pad`) or `interval_end` (the era's end plus `pad`), each
capped at the observation end when covered.

`omop_coverage()` is the observation-period policy: `unique` (exactly
one covering record; several follow `on_ambiguous`), `earliest` (one
complete record) or `union` (records coalesced with at most `gap`
unobserved days); `on_missing` decides uncovered indexes. The new
indexed-population default is `unique/drop/drop`.
