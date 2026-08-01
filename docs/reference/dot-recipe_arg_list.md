# Normalize a declarative slot argument to a list of items

Each
[`omop_recipe()`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
slot accepts a single building-block object (e.g. one `omop_output`) or
a list of them. This coerces both forms to a list so the delegation loop
can iterate uniformly. `NULL` and the leaf-object classes are wrapped
into a one-element list; an already-supplied list is returned unchanged
(preserving any element names, e.g. filter IDs).

## Usage

``` r
.recipe_arg_list(x)
```

## Arguments

- x:

  A single building-block object, a list of them, or `NULL`.

## Value

A list (possibly empty).
