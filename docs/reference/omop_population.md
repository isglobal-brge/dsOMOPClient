# Create a population node

A recipe defines one or more populations and every output targets one
(via `omop_output(..., population_id=)`). Each recipe starts with an
implicit `"base"` population representing all persons; additional
populations are passed to
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
via its `populations` argument.

## Usage

``` r
omop_population(
  id = "base",
  label = "Base Population",
  parent_id = NULL,
  filters = list(),
  cohort_definition_id = NULL,
  episode_policy = NULL,
  union = NULL,
  intersect = NULL,
  setdiff = NULL,
  index_event = NULL
)
```

## Arguments

- id:

  Character; population ID (must be unique within the recipe).

- label:

  Character; human-readable label.

- parent_id:

  Character or `NULL`; parent population ID (`NULL` for root).
  Informational provenance only; set-op membership is the executable
  dependency.

- filters:

  List of
  [`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md)
  or
  [`omop_filter_group`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter_group.md)
  objects (criteria populations only).

- cohort_definition_id:

  Integer or `NULL`; base cohort definition ID (if the population is
  defined by a pre-existing cohort).

- episode_policy:

  Character or `NULL`; explicit semantics for index-dependent filters
  when the index cohort can contain multiple episodes per person. One of
  `"any_episode"`, `"all_episodes"`, `"first_episode"`, or
  `"last_episode"`. Without a policy the server rejects index-dependent
  filtering of recurrent cohorts.

- union, intersect, setdiff:

  Character vector of two or more population IDs to derive this
  population from by the named set operation on the person key. Exactly
  one may be supplied, and only for a set-op population (mutually
  exclusive with `filters` / `cohort_definition_id`).

- index_event:

  An `omop_index_event` or `NULL`. When present, population filters are
  evaluated for each retained event episode and the event's start/end
  dates are preserved.

## Value

An `omop_population` object. A set-op population carries a
`$setop = list(op, members)` field; a criteria population carries
`$filters` (and optionally `$cohort_definition_id` and
`$episode_policy`).

## Details

A population is one of two kinds, which are mutually exclusive:

- **criteria-defined** — a person-level inclusion tree given in
  `filters` (any mix of
  [`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md)
  /
  [`omop_filter_group`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter_group.md)
  at the `"population"` level, e.g. sex + `has_concept` +
  `has_measurement`). It compiles to the same cohort filter tree the
  server builds the base cohort from.

- **set-op derived** — built from *other* populations by a set operation
  on the person key. Supply exactly one of `union`, `intersect`, or
  `setdiff` as a character vector of two or more population IDs; the
  server folds the named members with the matching algebra
  ([`ds.omop.cohort.combine`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.combine.md)'s
  `.cohortCombine`). `setdiff` keeps persons in the first member and not
  in the rest.

## See also

[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md),
[`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md),
[`ds.omop.cohort.combine`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.combine.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Criteria population.
females <- omop_population(id = "females", label = "Female patients",
                           filters = list(omop_filter_sex("F")))

# Set-op population: persons in EITHER of two criteria subgroups.
either <- omop_population(id = "either", label = "diabetic or hypertensive",
                          union = c("diabetic", "hypertensive"))
} # }
```
