# Generate reproducible R code from a recipe

Produces a minimal R script that, when executed, recreates the recipe
from scratch. The output is a single declarative
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)`(...)`
expression whose arguments are nested calls to the leaf constructors
(`omop_variable*`, `omop_filter*`, `omop_output`, `omop_population`,
`omop_variable_block`). Does not include
[`library()`](https://rdrr.io/r/base/library.html) calls or header
comments.

## Usage

``` r
recipe_to_code(recipe)
```

## Arguments

- recipe:

  An `omop_recipe` object.

## Value

Character string containing executable R code.

## See also

[`recipe_export_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_json.md),
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
code <- recipe_to_code(recipe)
cat(code)
} # }
```
