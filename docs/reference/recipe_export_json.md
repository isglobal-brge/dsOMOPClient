# Export a recipe to JSON

Serializes the recipe to a JSON string or file. All object classes are
stripped for clean serialization. The JSON format includes a schema
version tag (`"1"`) and can be re-imported with
[`recipe_import_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_import_json.md).

## Usage

``` r
recipe_export_json(recipe, file = NULL)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- file:

  Character or `NULL`; file path to write. If `NULL`, returns the JSON
  string directly.

## Value

If `file` is `NULL`, returns a JSON string (invisibly). Otherwise writes
to `file` and returns the file path invisibly.

## See also

[`recipe_import_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_import_json.md),
[`recipe_to_code`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_code.md)

## Examples

``` r
if (FALSE) { # \dontrun{
json <- recipe_export_json(recipe)
recipe_export_json(recipe, file = "my_recipe.json")
} # }
```
