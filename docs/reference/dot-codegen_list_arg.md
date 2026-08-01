# Wrap constructor-call code strings as a declarative slot argument

Used by
[`recipe_to_code`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_code.md)
to pass each
[`omop_recipe()`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
slot (populations, blocks, variables, outputs) a single nested
expression. A lone element is emitted bare (e.g. `omop_output(...)`);
multiple elements are wrapped in `list(...)`. Returns a
[`.codegen_raw()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-codegen_raw.md)
value so the enclosing
[`.codegen_call()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-codegen_call.md)
injects it verbatim.

## Usage

``` r
.codegen_list_arg(code_strings)
```

## Arguments

- code_strings:

  Character vector of constructor-call code strings.

## Value

A
[`.codegen_raw()`](https://isglobal-brge.github.io/dsOMOPClient/reference/dot-codegen_raw.md)
string, or `NULL` when empty.
