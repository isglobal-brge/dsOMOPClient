# Restore atomic vectors and integer types in a parsed plan

`jsonlite::fromJSON(simplifyVector = FALSE)` (and `yaml.load`) turn
atomic vectors into unnamed lists of scalars and read every number as a
double. This walks the parsed structure, collapses unnamed all-scalar
lists back to atomic vectors, and coerces the known integer fields
(`.plan_int_fields`) to integer, so the reconstructed plan re-encodes to
byte-identical transport JSON. Mirrors `.recipe_restore_params`.

## Usage

``` r
.plan_restore(x, key = NULL)
```

## Arguments

- x:

  A parsed plan substructure.

- key:

  Character; the name this node was stored under (drives integer
  coercion). `NULL` at the top level.

## Value

The normalized substructure.
