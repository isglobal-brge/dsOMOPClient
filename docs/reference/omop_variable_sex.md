# Create a sex (M/F) variable

Produces a derived variable that maps `gender_concept_id` to `"M"`
(8507) or `"F"` (8532).

## Usage

``` r
omop_variable_sex(name = "sex")
```

## Arguments

- name:

  Character; output column name (default `"sex"`).

## Value

An `omop_variable` object with `format = "sex_mf"` and a `$derived`
metadata field.

## See also

[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md),
[`omop_variable_age`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable_age.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(variables = omop_variable_sex(),
                      outputs = omop_output(type = "wide"))
} # }
```
