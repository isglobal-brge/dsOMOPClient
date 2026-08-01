# Export a recipe to YAML

Serializes the same portable recipe representation as
[`recipe_export_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_json.md)
to a YAML string or file.

## Usage

``` r
recipe_export_yaml(recipe, file = NULL)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- file:

  Character or `NULL`; file path to write. If `NULL`, returns the YAML
  string directly.

## Value

If `file` is `NULL`, returns a YAML string. Otherwise writes to `file`
and returns the file path invisibly.

## See also

[`recipe_import_yaml`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_import_yaml.md),
[`recipe_export_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_json.md)

## Examples

``` r
if (FALSE) { # \dontrun{
yaml <- recipe_export_yaml(recipe)
recipe_export_yaml(recipe, file = "my_recipe.yml")
} # }
```
