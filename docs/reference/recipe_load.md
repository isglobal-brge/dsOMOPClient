# Load a recipe from JSON or YAML

Convenience wrapper around `recipe_import_json` and
`recipe_import_yaml`. The parser is selected from the file extension.

## Usage

``` r
recipe_load(file)
```

## Arguments

  - file:

    Character; source path ending in `.json`, `.yml`, or `.yaml`.

## Value

An `omop_recipe` object.

## See also

`recipe_save`, `recipe_import_json`, `recipe_import_yaml`

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- recipe_load("analysis_recipe.yml")
} # }
```
