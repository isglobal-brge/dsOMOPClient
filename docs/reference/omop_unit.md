# Unit normalisation for an event's numeric value

Rows whose unit is the `target` keep their value; rows in a unit listed
in `convert` are converted with `value * factor + offset` (also applied
to `range_low`/`range_high`); conversion happens before every predicate
and reducer.

## Usage

``` r
omop_unit(
  target,
  convert = list(),
  unknown = c("missing", "exclude"),
  censored = c("keep", "exclude")
)
```

## Arguments

  - target:

    Target unit concept ID.

  - convert:

    Named list `"<unit_concept_id>" = c(factor = , offset = )`; `factor`
    finite and \> 0, `offset` finite (default 0).

  - unknown:

    Rows in any other unit: `"missing"` (value NA) or `"exclude"` (row
    dropped).

  - censored:

    Rows whose operator concept is an inequality (`<`, `>`, ...):
    `"keep"` the value or `"exclude"` the row.

## Value

An `omop_unit` object.
