# Normalise an aliasing spec so it survives the DataSHIELD JSON transport

Raw-column specs may be passed as named vectors to alias the output
columns, e.g. `c(sex = "gender_concept_id", "race_concept_id")`. Two
things must hold for the aliases to reach the server intact:

1.  No blank object keys: any element left unnamed is given a name equal
    to its value, so a partially named vector becomes fully named.

2.  A *named* spec must serialise as a JSON object, not an array.
    `jsonlite::toJSON(auto_unbox = TRUE)` drops the names of a named
    *atomic* vector (it emits a bare `[...]` array), but keeps the names
    of a *list*. So a named atomic vector is converted to a named list
    here.

A fully unnamed vector is returned unchanged (stays a plain array = no
aliasing). A spec that is already a list (e.g. a features spec) is left
alone.

## Usage

``` r
.fill_alias_names(x)
```

## Arguments

- x:

  A character vector (possibly partially named) or a list.

## Value

The normalised spec: unnamed vectors unchanged; named vectors filled and
returned as a named list.
