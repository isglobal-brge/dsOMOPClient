# Inspect the supported Circe exchange subset

The matrix distinguishes import, legacy export and native export.
Conditional support requires the stated subset; native expressibility
does not imply an export adapter. Inspection never executes a recipe or
queries a server.

## Usage

``` r
recipe_circe_support()
```

## Value

A static data frame of construct, import, legacy\_export,
native\_export, conditions, reason\_code, remedy and adapter\_version.
