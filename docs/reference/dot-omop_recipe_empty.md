# Build the empty recipe skeleton

Internal base used by
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md):
a fresh recipe with the implicit `"base"` population, empty slots, and
the default plan options. The declarative `omop_recipe(...)` starts from
this skeleton and delegates each supplied argument to the internal
slot-filling setters, so the declarative form is identical by
construction to incremental building.

## Usage

``` r
.omop_recipe_empty()
```

## Value

An empty `omop_recipe` object.
