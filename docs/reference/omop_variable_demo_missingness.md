# Create a demographics missingness variable

Produces a derived variable counting the number of missing or
zero-valued demographic fields per person (0-6 range).

## Usage

``` r
omop_variable_demo_missingness(name = "demo_missingness")
```

## Arguments

- name:

  Character; output column name (default `"demo_missingness"`).

## Value

An `omop_variable` with `format = "demo_missingness"`.
