# Row predicates over an event's columns

`omop_where()` captures an R expression as data (it is never evaluated)
using the closed grammar `==`, `!=`, `<`, `<=`, `>`, `>=`, `%in%`,
`is.na()`, `!`, `&`, `|` and parentheses, with literal numbers, strings
(ISO dates are strings), logical constants and concept IDs. Several
arguments are combined with AND. Bare names are column names; the
normalised value of an event is `value`. Logic is three-valued as in
SQL: a comparison with a NULL column is UNKNOWN and only TRUE rows are
kept.

## Usage

``` r
omop_where(...)

omop_where_tree(tree)
```

## Arguments

  - ...:

    Predicate expressions (unevaluated).

  - tree:

    A predicate tree.

## Value

An `omop_predicate` object.

## Details

A parameter placeholder, `omop_param(...)` written inline with literal
arguments, may stand for any literal; it is the only call ever evaluated
(alone, in the caller's frame) and is bound before emission.

`omop_where_tree()` takes the explicit tree form of the plan schema
(`list(var =, op =, value =)` leaves, `and`/`or` lists, `not`), which
also offers `between`.

## Examples

``` r
omop_where(value >= 6.5)
omop_where(!is.na(value), unit_concept_id %in% c(8554, 8753))
omop_where_tree(list(var = "value", op = "between",
                     value = list(lower = 5, upper = 10)))
```
