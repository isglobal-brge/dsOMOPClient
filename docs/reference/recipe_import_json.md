# Import a recipe from JSON

Reconstructs an `omop_recipe` from a JSON string or file previously
created by
[`recipe_export_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_json.md).
Automatically detects whether the input is a file path or a raw JSON
string. Nested filter groups, blocks, output options, population
filters, variable filters, and metadata are preserved.

## Usage

``` r
recipe_import_json(json)
```

## Arguments

- json:

  Character; a JSON string, or a file path to a JSON file.

## Value

An `omop_recipe` object.

## See also

[`recipe_export_json`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_json.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- recipe_import_json("my_recipe.json")
recipe <- recipe_import_json('{"version":"1","populations":{...}}')
} # }
```
