# Resolve a plan file's serialization format

Picks `"json"` or `"yaml"` from an explicit `format` or the file
extension (`.json` / `.yml` / `.yaml`). Mirrors `.recipe_file_format`.

## Usage

``` r
.plan_file_format(file, format = NULL)
```

## Arguments

- file:

  Character; the file path.

- format:

  Character or `NULL`; an explicit format override.

## Value

`"json"` or `"yaml"`.
