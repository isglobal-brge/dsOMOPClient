# Resolve a manipulation wrapper's target symbol, defaulting to the session

The data-manipulation verbs (`ds.omop.merge`, `ds.omop.filter`,
`ds.omop.select`, `ds.omop.bind_rows`) operate on the NAME of a
server-side `omop.table` symbol. When the caller omits it, fall back to
the session's `last_output` (the symbol most recently produced by
`ds.omop.plan.execute` / `recipe_execute`), so a user need not re-type
it. An explicit value always wins.

## Usage

``` r
.resolve_target_symbol(x, session, arg = "x")
```

## Arguments

  - x:

    The caller-supplied symbol name, or `NULL` to use the session
    default.

  - session:

    The `omop_session` object.

  - arg:

    Character; the argument name, used only in error messages.

## Value

A single character symbol name.
