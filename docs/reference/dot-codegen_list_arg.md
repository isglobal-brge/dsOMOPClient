# Wrap constructor-call code strings as a declarative slot argument

Used by `recipe_to_code` to pass each `omop_recipe()` slot (populations,
blocks, variables, outputs) a single nested expression. A lone element
is emitted bare (e.g. `omop_output(...)`); multiple elements are wrapped
in `list(...)`. Returns a `.codegen_raw()` value so the enclosing
`.codegen_call()` injects it verbatim.

## Usage

``` r
.codegen_list_arg(code_strings)
```

## Arguments

  - code\_strings:

    Character vector of constructor-call code strings.

## Value

A `.codegen_raw()` string, or `NULL` when empty.
