# Check a Circe definition without execution

Reports independent unsupported clauses in structural path order.
Malformed JSON, duplicate keys and exceeded size/depth limits produce a
root refusal. Arbitrary input values and exception payloads are never
included in issues. Import/export use this same checker and never return
a weakened definition.

## Usage

``` r
recipe_circe_check(x, direction = "import")
```

## Arguments

  - x:
    
    A JSON string, file or parsed list for import; a recipe for export.

  - direction:
    
    Exactly `"import"` or `"export"`.

## Value

A list with supported, adapter\_version, matrix\_version and issues.
