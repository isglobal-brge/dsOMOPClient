# Resolve the unified `cohort=` scope argument of the exploration wrappers

The exploration wrappers accept a single `cohort` argument naming the
population to scope to. This maps the accepted forms to the value the
SERVER's `.resolveCohortArg`/`.resolveCohortTable` expects, which then
materialises + re-gates it server-side:

- a `dsomop_cohort_handle` (from `ds.omop.cohort.create`, `.combine`, or
  `.from_table`) -\> its server TABLE name;

- a numeric cohort_definition_id -\> the integer, passed through so the
  server materialises it from the cohort results table;

- a character TABLE name -\> as-is;

- `NULL` -\> `NULL` (no scoping).

This deliberately does NOT collapse a numeric id to a
`dsomop_cohort_<id>` temp-table name (that is `.resolve_cohort_table`'s
job for the set-ops path); for exploration a bare id means a
cohort_definition_id.

## Usage

``` r
.cohort_scope_arg(cohort)
```

## Arguments

- cohort:

  The unified `cohort` argument.

## Value

A server-side cohort table name, a cohort_definition_id, or NULL.
