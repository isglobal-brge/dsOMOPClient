# Population criteria

Criteria are population predicates evaluated per person or per episode
with three-valued logic: a criterion over an undefined window (a
required anchor is missing) is UNKNOWN for every quantifier, UNKNOWN
propagates through `omop_not`/`omop_all`/`omop_any`, and only TRUE
survives the population boundary.

## Usage

``` r
omop_has(
  event,
  n = 1L,
  op = ">=",
  window = NULL,
  distinct = c("row", "day", "visit", "concept")
)

omop_has_not(event, window = NULL, nonmissing = NULL)

omop_count_is(
  event,
  op,
  n,
  window = NULL,
  distinct = c("row", "day", "visit", "concept")
)

omop_not(criterion)

omop_all(...)

omop_any(...)

omop_person(where)

omop_age_is(min = NULL, max = NULL, reference = "index", year = NULL)

omop_observed(before = NULL, after = NULL, gap = NULL)

omop_in_cohort(cohort_definition_id)
```

## Arguments

  - event:

    An `omop_event`, an `omop_era` or a registry name.

  - n:

    Count threshold (`c(min, max)` for `between`).

  - op:

    Comparison: `">="`, `"<="`, `"=="` or `"between"`.

  - window:

    `NULL` (any time) or an `omop_window`.

  - distinct:

    What to count: `"row"`, `"day"`, `"visit"` or `"concept"`.

  - nonmissing:

    `NULL` or a column that must be non-null.

  - criterion:

    A criterion.

  - ...:

    Criteria to combine.

  - where:

    A predicate from `omop_where`.

  - min, max:

    Age bounds (inclusive; `NULL` = open).

  - reference:

    `"index"`.

  - year:

    Optional fixed reference year (instead of the index).

  - before, after:

    Required observed days before/after the index.

  - gap:

    `NULL` or tolerated unobserved days between records.

  - cohort\_definition\_id:

    A results-cohort definition ID.

## Value

An `omop_criterion` object.

## Details

  - `omop_has(event, n, op, window, distinct)`: at least `n` (or `op n`)
    rows of the event in the window; `distinct` counts rows, distinct
    days, visits (NULL visit keys ignored) or concepts.

  - `omop_has_not(event, window, nonmissing)`: no row (or, with
    `nonmissing = "value_as_number"`, no row with a non-null value).

  - `omop_count_is(event, op, n, window, distinct)`: a count comparison;
    `op = "between"` takes `n = c(min, max)`.

  - `omop_not`, `omop_all`, `omop_any`: logic.

  - `omop_person(where)`: a person-table predicate (birth components may
    be referenced directly).

  - `omop_age_is(min, max, reference, year)`: annual age (reference year
    minus year of birth) at the index (or in `year`).

  - `omop_observed(before, after, gap)`: observation coverage of at
    least `before` days before and `after` days after the index (`gap`
    tolerates unobserved days between records).

  - `omop_in_cohort(cohort_definition_id)`: membership of a results
    cohort.

## Examples

``` r
omop_all(omop_observed(before = 365),
         omop_has_not(omop_event("drug_exposure", 1503297), window = omop_window(-365, -1)))
```
