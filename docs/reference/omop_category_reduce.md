# Reduce explicitly transformed event category identities

Reduce explicitly transformed event category identities

## Usage

``` r
omop_category_reduce(
  variable,
  reducer,
  category_spec = NULL,
  broadcast = FALSE
)
```

## Arguments

  - variable:
    
    Typed event category or indicator projection.

  - reducer:
    
    Existing typed selector, mode, count, distinct or flag reducer.

  - category\_spec:
    
    Optional final presentation (after reduction).

  - broadcast:
    
    Explicit scalar broadcast.

## Value

A scalar variable declaration (or the reducer's nth-value columns).
