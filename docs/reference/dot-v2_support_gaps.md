# Operations a v2 plan needs that each server lacks

Operations a v2 plan needs that each server lacks

## Usage

``` r
.v2_support_gaps(plan, session, servers, extra = character(0))
```

## Arguments

  - plan:

    A v2 plan.

  - session:

    The OMOP session.

  - servers:

    Server names.

  - extra:

    Additional operations required for execution.

## Value

Named list: per server, the missing operations (empty when ok).
