# Add cardinality-safe person-level tables to the plan

Merges one-row-per-person sources by `person_id`. Raw columns are
accepted only from tables whose cardinality is guaranteed to be at most
one row per person; repeatable clinical tables must be reduced through
explicit feature specifications. For cohort-aware demographics and age
at index, use
[`ds.omop.plan.baseline`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.baseline.md)
instead.

## Usage

``` r
ds.omop.plan.person_level(plan, tables, name = "person_data")
```

## Arguments

- plan:

  An `omop_plan` object.

- tables:

  Named list; each element maps a table name to a character vector of
  column names to include, e.g.
  `list(person = c("gender_concept_id", "race_concept_id"))`. Each
  column vector may be *named* to rename columns in the output:
  `c(sex = "gender_concept_id", race = "race_concept_id")` yields output
  columns `sex` and `race`. Unnamed entries keep their source name.
  Identifier columns cannot be renamed (they are stripped server-side
  regardless).

- name:

  Character; output name used as a key in the plan's outputs list and as
  the default symbol name on the server.

## Value

The modified `omop_plan` with the person-level output appended.

## See also

[`ds.omop.plan.baseline`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.baseline.md),
[`ds.omop.plan.events`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.events.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.person_level(plan,
  tables = list(
    person = c("gender_concept_id", "race_concept_id")
  ),
  name = "demographics"
)

# Rename columns at request time with a named vector
plan <- ds.omop.plan.person_level(plan,
  tables = list(person = c(sex = "gender_concept_id",
                           race = "race_concept_id")),
  name = "demographics"
)
} # }
```
