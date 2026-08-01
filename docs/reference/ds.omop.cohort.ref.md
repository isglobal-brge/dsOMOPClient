# Create a cohort reference for the plan DSL (client-only)

Builds a lightweight cohort reference object that can be embedded in a
plan specification. This is a client-side-only helper that does not
contact any server; the reference is resolved at plan execution time
when
[`recipe_execute()`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md)
runs on the server.

## Usage

``` r
ds.omop.cohort.ref(cohort_definition_id)
```

## Arguments

- cohort_definition_id:

  Integer; the ID of an existing cohort definition that has already been
  created on the server(s). Referencing an absent or sub-threshold id is
  accepted here but fails closed when resolved server-side (see
  Disclosure control).

## Value

A named list with class-implicit structure containing
`type = "cohort_table"` and `cohort_definition_id`. Intended for use
inside plan population specifications.

## Disclosure control

Building the reference does no checking (it never touches a server); the
disclosure gate applies when the reference is RESOLVED at execution
time. If the cohort_definition_id is unavailable – absent OR below the
server's per-subset threshold (`nfilter_subset`) – materialising/using
it FAILS CLOSED server-side (the same fail-closed gate that protects
every other way of naming a cohort, e.g. the `cohort=` scope of the
exploration wrappers). A sub-threshold cohort can therefore never be
used to scope or populate a query, exactly as if it did not exist.

## Examples

``` r
if (FALSE) { # \dontrun{
ref <- ds.omop.cohort.ref(cohort_definition_id = 1)
# Use in a plan: plan$population <- ref
} # }
```
