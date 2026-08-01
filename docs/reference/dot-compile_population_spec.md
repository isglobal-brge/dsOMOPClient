# Serialize one population for the server's multi-population plan

Each
[`omop_population`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_population.md)
compiles to a transport-safe spec the server materializes and gates
independently. A set-op population carries its `list(op, members)`
verbatim (the server folds the named members with the matching cohort
algebra). A criteria population compiles its person-level filter chain
to the SAME nested AND/OR `filter_tree` the base cohort uses (via
[`.compile_population_filter_tree`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-compile_population_filter_tree.md)),
so the server reuses its existing `.buildCohortFromFilters` path, and
carries any `cohort_definition_id` and `episode_policy`.
`filter_tree`/`cohort_definition_id`/`episode_policy` are included only
when set so a bare population stays compact.

## Usage

``` r
.compile_population_spec(pop)
```

## Arguments

- pop:

  An `omop_population` object.

## Value

A named list spec with `id`, `label`, `kind` (`"setop"` or
`"criteria"`), and the kind-specific fields.
