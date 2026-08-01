# Resolve a cohort reference to its server-side table name

Maps the various forms a caller may supply for a cohort – a
`dsomop_cohort_handle` (as returned by `ds.omop.cohort.create` or
`ds.omop.cohort.combine`), a cohort definition ID, or a server-side name
string – to the deterministic temp table name the server expects.

## Usage

``` r
.resolve_cohort_table(x)
```

## Arguments

- x:

  A `dsomop_cohort_handle`, a numeric cohort definition ID, or a
  character table/symbol name.

## Value

Character; the server-side cohort table name.
