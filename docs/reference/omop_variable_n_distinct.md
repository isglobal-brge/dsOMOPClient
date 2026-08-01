# Create a distinct-concept-count variable

Produces a feature variable that counts the number of distinct concept
IDs per person in the specified table.

## Usage

``` r
omop_variable_n_distinct(table, name = NULL)
```

## Arguments

- table:

  Character; source OMOP CDM table (e.g. `"condition_occurrence"`).

- name:

  Character or `NULL`; output column name (auto-generated as
  `"n_distinct_<table>"` if `NULL`).

## Value

An `omop_variable` object with `format = "n_distinct"` and a `$derived`
metadata field.

## See also

[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md),
[`omop.feature.n_distinct`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.n_distinct.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  variables = omop_variable_n_distinct("condition_occurrence"),
  outputs = omop_output(type = "wide"))
} # }
```
