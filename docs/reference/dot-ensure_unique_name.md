# Ensure a variable name is unique within existing names

Appends numeric suffixes (`_2`, `_3`, etc.) to resolve collisions with
existing names. Also escapes R reserved words by appending `_var`.

## Usage

``` r
.ensure_unique_name(name, existing)
```

## Arguments

- name:

  Character; proposed name.

- existing:

  Character vector; existing names to check against.

## Value

Character; a unique name (with `_2`, `_3` suffixes if needed).
