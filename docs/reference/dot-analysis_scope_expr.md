# Build named server-side scope arguments for an analysis run

A single cohort reference travels as the literal named argument `scope`;
multiple references travel as separate scalar `scope_cohort_1`,
`scope_cohort_2`, ... arguments. Workspace `omop.table` symbols travel
separately as bare-symbol named arguments `scope_table_1`,
`scope_table_2`, and so on. No nested
[`list()`](https://rdrr.io/r/base/list.html) or
[`c()`](https://rdrr.io/r/base/c.html) expression is emitted: allowing
those generic AggregateMethods would let a caller evaluate an unreviewed
container around protected objects.

## Usage

``` r
.analysis_scope_expr(cohort = NULL, tables = NULL)
```

## Arguments

- cohort:

  Cohort reference (a `dsomop_cohort_handle`, a `cohort_definition_id`,
  or a server-side table name) or `NULL`.

- tables:

  Character vector of server-side `omop.table` symbol names, or `NULL`.

## Value

`NULL` or a named local list of call arguments. Its values are cohort
literals and/or bare table symbols; it never contains a call.

## Details

Forms produced:

- no cohort, no tables -\> `NULL` (no scoping argument).

- one cohort -\> `list(scope = <literal>)`; multiple cohorts -\>
  sequential scalar `scope_cohort_<n>` arguments.

- table symbol(s) -\> a named list whose values are bare symbols and
  whose names are the sequential `scope_table_<n>` arguments.
