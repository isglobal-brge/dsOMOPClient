# Resolve the out argument of ds.omop.plan.execute into a named mapping

Normalises the three accepted `out` forms into the named
`output -> symbol` character vector the server expects. `NULL`
auto-derives a symbol for every plan output exactly as
[`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md)
does (an output's `result_symbol` when set, else `D_<name>`). A bare
unnamed string is bound to the plan's sole output, or stops with an
instructive error when the plan has several outputs. A named vector is
validated and returned unchanged.

## Usage

``` r
.resolve_plan_out(plan, out, reserved_symbols = character(0))
```

## Arguments

- plan:

  An `omop_plan` object.

- out:

  `NULL`, a bare unnamed string, or a named character vector.

- reserved_symbols:

  Optional server symbols that outputs may not replace.

## Value

A named character vector mapping output names to server symbols.
