# Resolve a manipulation wrapper's target symbol, defaulting to the session

The data-manipulation verbs
([`ds.omop.merge`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.merge.md),
[`ds.omop.filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.filter.md),
[`ds.omop.select`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.select.md),
[`ds.omop.bind_rows`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.bind_rows.md))
operate on the NAME of a server-side `omop.table` symbol. When the
caller omits it, fall back to the session's `last_output` (the symbol
most recently produced by
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)
/
[`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md)),
so a user need not re-type it. An explicit value always wins.

## Usage

``` r
.resolve_target_symbol(x, session, arg = "x")
```

## Arguments

- x:

  The caller-supplied symbol name, or `NULL` to use the session default.

- session:

  The `omop_session` object.

- arg:

  Character; the argument name, used only in error messages.

## Value

A single character symbol name.
