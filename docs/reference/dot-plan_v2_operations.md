# Language operations a v2 plan needs from a server

The same inventory the server computes (`.irOperations()`), over the
normal form of the plan, so the client sends a v2 plan exactly when
every server advertises what it needs (design section 4.4).

## Usage

``` r
.plan_v2_operations(plan)
```

## Arguments

  - plan:

    A v2 plan.

## Value

A sorted character vector of operation names and, as attribute
`"reducers"`, the reducer functions used.
