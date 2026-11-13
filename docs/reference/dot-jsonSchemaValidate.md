# Validate an instance against a schema (2020-12 subset)

Validate an instance against a schema (2020-12 subset)

## Usage

``` r
.jsonSchemaValidate(schema, instance, root = schema, path = "")
```

## Arguments

  - schema:

    Parsed schema (list) or a boolean schema.

  - instance:

    Parsed instance.

  - root:

    Root schema used to resolve local references.

  - path:

    JSON pointer of the instance being validated (for messages).

## Value

list(valid = logical(1), errors = character()) where each error is
"\<instance pointer\>: \<message\>".
