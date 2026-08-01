# Sanitize a concept name into a valid R variable name

Converts a human-readable concept name into a safe R variable name by
lowercasing, replacing non-alphanumeric characters with underscores, and
truncating to 50 characters.

## Usage

``` r
.sanitize_name(x)
```

## Arguments

- x:

  Character; the name to sanitize.

## Value

Character; a valid R variable name.
