# Reviewed identifier names used by the assignment compiler

Mirrors the server's reviewed CDM 5.3/5.4 and analytic identifier names.
These names have typed semantics; unfamiliar id/key/identifier
extensions remain blocked. This does not retain row identifiers in
assigned outputs: the server applies its own pseudonymization and
identifier-removal contract.

## Usage

``` r
.assignment_reviewed_identifiers()
```
