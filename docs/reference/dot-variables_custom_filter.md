# Build a custom filter tree from a set of variables' row-level filters

Collects every `"row"`-level `omop_filter` attached to the given
variables (`v$filters`) and compiles them into a single AND/OR tree in
the server's `.compileFilter()` DSL. This is what carries per-variable
row filters (e.g. `value_bin`, `date_range`) from the recipe into the
plan's `output$filters$custom`, where the server validates them
fail-closed and ANDs them into the extraction WHERE.

## Usage

``` r
.variables_custom_filter(vars, table = NULL)
```

## Arguments

- vars:

  List of `omop_variable` objects (typically one table's).

- table:

  Character or `NULL`; source OMOP table.

## Value

A filter-tree list, or `NULL` if no row filters are present.
