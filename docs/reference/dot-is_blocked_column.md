# Is a column name a server-blocked free-text / source-value column?

Mirrors the server's blocked-column patterns (dsOMOP blueprint:
`*_source_value`, `value_as_string`, `value_source_value`, `sig`,
`*_source_concept_value`, note text). These are never releasable, so a
recipe that requests one as a variable's `value_source` or raw `column`
is rejected at compile time rather than silently dropped.

## Usage

``` r
.is_blocked_column(col)
```

## Arguments

- col:

  Character; a column name (case-insensitive).

## Value

Logical scalar.
