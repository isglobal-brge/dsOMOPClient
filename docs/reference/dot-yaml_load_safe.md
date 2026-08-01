# Parse YAML without evaluating embedded R expressions

The yaml package can be configured globally to evaluate \`!expr\` tags.
The client never accepts executable YAML, irrespective of that
process-wide option. A sentinel handler lets us reject such tags
explicitly while still relying on yaml's parser (instead of attempting
to recognise tags with a regular expression).

## Usage

``` r
.yaml_load_safe(text)
```

## Arguments

- text:

  A length-one YAML document.

## Value

The parsed R object.
