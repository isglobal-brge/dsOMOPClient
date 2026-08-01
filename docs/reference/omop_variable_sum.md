# Create a sum variable

Produces a feature variable that sums a numeric column per person for
records matching the concept set.

## Usage

``` r
omop_variable_sum(
  table,
  column,
  concept_id = NULL,
  concept_name = NULL,
  name = NULL
)
```

## Arguments

- table:

  Character; source OMOP CDM table.

- column:

  Character; numeric column to sum (e.g. `"days_supply"`, `"quantity"`).

- concept_id:

  Integer or `NULL`; concept ID filter.

- concept_name:

  Character or `NULL`; human-readable name.

- name:

  Character or `NULL`; output column name (auto-generated if `NULL`).

## Value

An `omop_variable` object with `format = "sum"` and a `$derived`
metadata field.

## See also

[`omop_variable`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_variable.md),
[`omop.feature.sum_value`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop.feature.sum_value.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  variables = omop_variable_sum("drug_exposure", "days_supply",
                                concept_id = 1124300),
  outputs = omop_output(type = "wide"))
} # }
```
