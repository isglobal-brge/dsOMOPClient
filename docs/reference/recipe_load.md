# Load a recipe from JSON or YAML

Convenience wrapper around
[`recipe_import_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_import_json.md)
and
[`recipe_import_yaml`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_import_yaml.md).
The parser is selected from the file extension.

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

[`recipe_save`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_save.md),
[`recipe_import_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_import_json.md),
[`recipe_import_yaml`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_import_yaml.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- recipe_load("analysis_recipe.yml")
} # }
```
