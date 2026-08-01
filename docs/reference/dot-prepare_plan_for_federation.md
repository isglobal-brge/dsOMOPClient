# Prepare a plan for the exact federation used by an operation

Multi-server validate, preview and execute all use the same strict
schema and semantic binding. A newly harmonized contract is checked
structurally and against the plan signature; a pre-existing contract is
fully re-introspected.

## Usage

``` r
.prepare_plan_for_federation(plan, symbol, conns)
```

## Arguments

- plan:

  An `omop_plan`.

- symbol:

  Client OMOP session name.

- conns:

  DSI connections.

## Value

The prepared plan.
