# Save a recipe to JSON or YAML

Convenience wrapper around
[`recipe_export_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_json.md)
and
[`recipe_export_yaml`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_yaml.md).
The format is inferred from the file extension unless supplied
explicitly.

## Usage

``` r
recipe_save(recipe, file, format = NULL)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- file:

  Character; destination path ending in `.json`, `.yml`, or `.yaml`.

- format:

  Character or `NULL`; optional explicit format: `"json"` or `"yaml"`.

## Value

The file path invisibly.

## See also

[`recipe_load`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_load.md),
[`recipe_export_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_json.md),
[`recipe_export_yaml`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_yaml.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe_save(recipe, "analysis_recipe.yml")
recipe_save(recipe, "analysis_recipe.json")
} # }
```
