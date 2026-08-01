# Map a human domain name (or code) to the catalog domain_code

The covariate analyses select their event family with a `domain_code`
("0" condition, "1" drug, ...). Accept the friendly domain NAME as well
so a caller writes `domain = "condition"` instead of memorising the
code; a code passed through unchanged.

## Usage

``` r
.analysis_domain_code(domain = NULL, default = "0")
```

## Arguments

- domain:

  Character/numeric domain name or code, or `NULL`.

- default:

  Character; the code to use when `domain` is `NULL`.

## Value

Character domain code.
