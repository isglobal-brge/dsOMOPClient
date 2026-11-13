# Arithmetic on numeric parameter placeholders

`+`, `-`, `*` and `/` (and unary minus) combine an integer or number
`omop_param` with numeric literals or other numeric parameters into a
placeholder expression, evaluated when its parameters are bound. No
other operator applies to a parameter.

## Usage

``` r
# S3 method for class 'omop_param'
Ops(e1, e2)
```

## Arguments

  - e1, e2:

    Operands.

## Value

A parameter expression (class `"omop_param_op"`).
