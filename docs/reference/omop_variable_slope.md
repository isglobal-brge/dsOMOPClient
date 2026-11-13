# Create a slope (linear trend) variable

Produces a feature variable fitting a linear model of value over time
per person and extracting the slope.

## Usage

``` r
omop_variable_slope(
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

An `omop_variable` with `format = "slope"`.
