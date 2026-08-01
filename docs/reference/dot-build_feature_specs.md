# Build feature specifications from a list of omop_variable objects

Maps each variable's format to the corresponding `omop.feature.*`
constructor and returns a named list of feature spec objects.

## Usage

``` r
.build_feature_specs(vs)
```

## Arguments

- vs:

  List of `omop_variable` objects.

## Value

Named list of feature spec objects.
