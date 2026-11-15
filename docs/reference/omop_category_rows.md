# Event category projections and identity indicators

Event category projections and identity indicators

## Usage

``` r
omop_category_rows(event, value = NULL, category_spec = NULL)

omop_category_indicators(variable, keys, missing = "propagate")
```

## Arguments

  - event:
    
    Typed event or registered event name.

  - value:
    
    Registered concept-valued role; NULL selects the primary concept.

  - category\_spec:
    
    Explicit category presentation.

  - variable:
    
    A category variable declaration.

  - keys:
    
    Named stable category keys; names are suffixes.

  - missing:
    
    propagate or zero for ordinary available NA.

## Value

A variable declaration or named immutable bundle of declarations.
