# First non-NULL element of a list

Returns the first element of `x` that is not `NULL`, or `NULL` if every
element is `NULL`. Used to pick a single value (e.g. a visit filter or
concept-scope column) from a set of variables, "first one set wins".

## Usage

``` r
.first_non_null(x)
```

## Arguments

- x:

  A list.

## Value

The first non-NULL element, or `NULL`.
