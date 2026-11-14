# Composable calendar presentation and anchored ages

Date declarations execute privately on each server. Calendar periods are
ordered factors by default; number supplies trend coordinates, and
start/end supplies inclusive period bounds. Exact ages use completed
anniversaries (February 29 uses March 1 in non-leap years). Missing
partial dates give NA unless a policy is declared. All patient-linked
results retain class 2. Variable fields override recipe fields, then
design defaults.

## Usage

``` r
omop_date_spec(
  precision = NULL,
  representation = NULL,
  numeric = NULL,
  origin = NULL,
  anchor = NULL,
  pattern = NULL,
  labels = NULL,
  level_range = NULL,
  fiscal_start = NULL,
  week_start = NULL,
  week_min_days = NULL,
  hemisphere = NULL,
  partial = NULL,
  invalid = NULL,
  future = NULL,
  as_of = NULL
)

omop_date(reference, date_spec = NULL, broadcast = FALSE)

omop_date_components(
  year,
  month = NULL,
  day = NULL,
  event = NULL,
  which = "first",
  n = 1L,
  scope = "person",
  search = NULL
)

omop_date_parts(
  reference,
  parts = c("year", "month", "day"),
  date_spec = NULL,
  broadcast = FALSE
)

omop_time_between(
  from,
  to,
  unit = "days",
  absolute = FALSE,
  rounding = "none",
  date_spec = NULL,
  broadcast = FALSE
)

omop_age_at(
  reference = "index",
  unit = "years",
  bands = FALSE,
  age_breaks = NULL,
  date_spec = NULL,
  broadcast = FALSE
)
```

## Arguments

  - precision:

    Calendar period, day, relative\_days or custom.

  - representation:

    auto, factor, number, start or end.

  - numeric:

    index or decimal\_year period coordinate.

  - origin:

    Fixed ISO origin for numeric period indices (default 1970-01-01).

  - anchor:

    Relative-days anchor.

  - pattern:

    Closed calendar pattern; ISO weeks require %G-W%V.

  - labels:

    Named public calendar label map; labels must remain unique.

  - level\_range:

    Two inclusive public ISO dates declaring factor periods.

  - fiscal\_start:

    Fiscal starting month (1..12).

  - week\_start, week\_min\_days:

    Explicit epidemiological week profile (1..7).

  - hemisphere:

    north or south, for meteorological seasons.

  - partial:

    none, start, mid or end, for incomplete dates.

  - invalid:

    na (invalid row values remain missing).

  - future:

    allow or na; na requires a fixed as\_of.

  - as\_of:

    Fixed ISO future-policy cutoff.

  - reference, from, to:

    Date sources: anchor, ISO date, index, index\_end,
    observation\_start/end, birth, death, first\_visit or last\_visit.

  - date\_spec:

    Optional omop\_date\_spec declaration.

  - broadcast:

    Repeat a roster value in long event rows.

  - year, month, day:

    Integer literals or registered integer source columns.

  - event:

    Optional event source for components (default person).

  - which, n, scope, search:

    Existing anchor row-selection semantics.

  - parts:

    Unique date parts; named entries supply exact final column names.

  - unit:

    days, weeks, months, years or decimal\_years; age also
    annual\_years.

  - absolute:

    Absolute elapsed time (default signed to minus from).

  - rounding:

    none, floor, ceiling, trunc or nearest (ties to even).

  - bands:

    Return ordered age bands (years/annual\_years only).

  - age\_breaks:

    Integer lower bounds beginning at zero; default 0,5,...,100.

## Value

A date specification, source, variable or variable bundle.

## Details

Calendar precisions are day, year, semester, quarter, month, iso\_week,
cdc\_week, epi\_week, fiscal\_year, fiscal\_semester, fiscal\_quarter,
fiscal\_month, relative\_days and custom. ISO weeks start Monday with
four new-year days in week 1; CDC weeks start Sunday with four.
epi\_week requires both week\_start and week\_min\_days. Fiscal year
labels name the starting year. Numeric periods count from the period
containing origin (zero); year and fiscal\_year instead return the year.
decimal\_year is a period coordinate, not an elapsed-year fraction.
start/end return inclusive period bounds.

Parts are year, semester, quarter, month, month\_name, month\_abbr, day,
weekday, weekday\_name, weekday\_abbr, iso\_week, iso\_year,
day\_of\_year, epi\_week, epi\_year, fiscal\_year, fiscal\_quarter,
season, month\_sin, month\_cos, weekday\_sin, weekday\_cos, season\_sin
and season\_cos. Month/weekday/season categories have full ordered
public domains. Names use fixed English. Monday is weekday 1. Northern
seasons are Winter, Spring, Summer, Autumn (December-February first);
southern names are Summer, Autumn, Winter, Spring. Cyclic values use
2\*pi\*(k-1)/P with P=12, 7 or 4. Ordered factors use ordered-factor
contrasts; request number for a trend coordinate. Shared unused periods
are zero-count levels.

Custom patterns accept only %Y, %m, %d, %j, %G, %V, %u, %b, %B, %a, %A,
%% and ASCII letters, digits, space or -\_/.,:(). ISO %G and %V must
appear together without %Y. No locale, code or time-of-day directives
are evaluated. Public labels must be unique and cannot merge date
categories. level\_range declares additional levels and never filters
observed dates.

Gregorian dates cover 0001-01-01 through 9999-12-31. Partial dates with
partial=none retain only invariant coarsened periods/parts; exact age is
NA. start/mid/end impute the first/middle/last possible day and produce
estimated ages. Invalid components cannot be repaired. annual\_years
explicitly returns calendar-year age, including partial births. Negative
age is NA. Elapsed months clamp the original day; elapsed years use
completed anniversaries; decimal years divide the remainder by the next
anniversary interval. A fixed as\_of is required for future=na. Bind a
fixed date instead of today to make the plan reproducible. Missing
anchors retain all roster rows.

An explicit empty spec selects native Date presentation for first\_date
and last\_date reducers; an omitted spec preserves legacy offsets.
Native helpers need language 2 at every selected site. Explicit
overlapping date\_handling refuses; use date\_spec for that policy. No
owner date permission is required on assignment. Existing publication,
metadata support and aggregate gates still apply. Only symbols and
disclosure-checked aggregates reach analysts.

## Examples

``` r
omop_age_at("death")
omop_date("first_visit", omop_date_spec(precision = "quarter"))
omop_date_parts("first_visit", c(visit_year = "year", visit_month = "month"))
```
