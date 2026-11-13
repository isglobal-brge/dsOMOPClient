# Lint a schema: every branch may use only the supported keywords

Lint a schema: every branch may use only the supported keywords

## Usage

``` r
.jsonSchemaLint(schema, path = "#")
```

## Arguments

  - schema:

    Parsed schema.

  - path:

    Pointer used in messages.

## Value

Invisibly TRUE; stops on the first unsupported keyword.
