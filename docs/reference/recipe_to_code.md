# Generate reproducible R code from a recipe

Produces a minimal R script that, when executed, recreates the recipe
from scratch. The output starts with a declarative `omop_recipe(...)`
expression whose arguments are nested calls to the leaf constructors
(`omop_variable*`, `omop_filter*`, `omop_output`, `omop_population`,
`omop_variable_block`). When block members have been edited, explicit
corrections preserve their names, order, definitions and provenance.
Does not include `library()` calls or header comments.

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

`recipe_export_json`, `omop_recipe`

## Examples

``` r
if (FALSE) { # \dontrun{
code <- recipe_to_code(recipe)
cat(code)
} # }
```
