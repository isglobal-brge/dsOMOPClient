# Import a recipe from YAML

Reconstructs an `omop_recipe` from a YAML string or file previously
created by
[`recipe_export_yaml`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_yaml.md).

## Usage

``` r
recipe_import_yaml(yaml)
```

## Arguments

- yaml:

  Character; a YAML string, or a file path to a YAML file.

## Value

An `omop_recipe` object.

## See also

[`recipe_export_yaml`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_yaml.md),
[`recipe_import_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_import_json.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- recipe_import_yaml("my_recipe.yml")
} # }
```
