# Collapse numeric observations by recorded calendar day

Collapse numeric observations by recorded calendar day

## Usage

``` r
omop_number_daily(
  input,
  reducer,
  within_day = "mean",
  missing = "skip",
  number_spec = NULL
)
```

## Arguments

  - input:
    
    Numeric event rows or an unreduced same-event numeric expression.

  - reducer:
    
    Explicit numeric omop\_reduce declaration, with value/unit omitted.

  - within\_day:
    
    mean, max or first in canonical event order.

  - missing:
    
    skip or propagate within each day, independently of the outer
    reducer.

  - number\_spec:
    
    Final numeric presentation after both reductions.

## Value

An ordinary numeric variable requiring the number.daily capability.

## Details

Selection precedes grouping by person/consumer episode, source, concept,
normalized coordinate and recorded date. Input presentation precedes
collapse. No absent days are filled. Selected-row ranges and visit
identities are discarded, including for first. Multiple concepts require
an explicitly declared common coordinate; they remain separate
concept-day observations.
