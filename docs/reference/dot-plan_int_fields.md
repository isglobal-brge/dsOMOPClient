# Integer field names that must survive a plan round-trip

JSON/YAML parsing loses the integer/double distinction and turns short
atomic vectors into lists of scalars. These are the plan fields the
builders store as integers (concept ids, cohort ids, offsets, bin
geometry); `.plan_restore` coerces them back so a save/load/execute
round-trip sends the server the identical payload.

## Usage

``` r
.plan_int_fields
```

## Format

An object of class `character` of length 17.
