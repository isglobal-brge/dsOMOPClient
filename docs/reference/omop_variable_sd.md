# Create a standard deviation variable

Produces a feature variable computing the standard deviation of a
numeric column per person for records matching the concept.

## Usage

``` r
omop_variable_sd(
  table,
  concept_id,
  concept_name = NULL,
  name = NULL,
  value_source = "value_as_number"
)
```

## Arguments

  - table:

    Character; source OMOP CDM table.

  - concept\_id:

    Integer; concept ID filter.

  - concept\_name:

    Character or `NULL`; human-readable name.

  - name:

    Character or `NULL`; output column name.

  - value\_source:

    Character; value column (default `"value_as_number"`).

## Value

An `omop_variable` with `format = "sd"`.
