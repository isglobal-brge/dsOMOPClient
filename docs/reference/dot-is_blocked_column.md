# Is a column blocked on assignment paths?

Mirrors the server's class-1 patterns, including untyped identifiers.
Birth components are permitted; an independent class-1 match still
blocks names such as `birth_date_source_value` or `mrn_birth_date`.
Aggregate admission is a separate server policy.

## Usage

``` r
.is_blocked_column(col)
```

## Arguments

  - col:

    Character; a column name (case-insensitive).

## Value

Logical scalar.
