# Apply a JSON Schema's \`default\` keywords to an instance

Defaults are added in the schema branch the instance matches
(properties, additionalProperties, items, allOf, the first matching
oneOf/anyOf branch, if/then/else), recursively. Nothing else is added.

## Usage

``` r
.pnv2ApplyDefaults(schema, x, root)
```

## Arguments

  - schema:

    Parsed schema node.

  - x:

    Instance (as parsed with \`simplifyVector = FALSE\`).

  - root:

    Root schema for \`$ref\` resolution.

## Value

The instance with every absent defaulted property present.
