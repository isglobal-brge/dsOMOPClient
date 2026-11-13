# Canonical JSON (RFC 8785) of an R value parsed with simplifyVector = FALSE

Objects are named lists, arrays unnamed lists, scalars length-one
vectors, null is NULL. Atomic vectors of length other than one are
serialised as arrays of scalars.

## Usage

``` r
.jcsCanonical(x)
```
