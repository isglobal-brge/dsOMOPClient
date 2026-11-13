# Which execution protocol the participating sites support

Which execution protocol the participating sites support

## Usage

``` r
.plan_protocol_route(
  conns,
  server_method = "omopPlanExecuteDS",
  harmonizes = FALSE
)
```

## Arguments

  - conns:

    DSI connections of the execution.

  - server\_method:

    The assign method the execution calls (`omopPlanExecuteDS` or
    `omopDpPlanExecuteDS`).

  - harmonizes:

    Whether the execution harmonises concept factors afterwards
    (`omopFactorLevelsDS`, `omopAsFactorColumnsDS`).

## Value

`list(protocol, old, missing)`: `protocol` is TRUE when every site
publishes the publication protocol; `old` names the sites that do not
and `missing` their missing protocol methods. A site whose profile
cannot be listed, or that lacks a method the call needs on any route,
stops the execution before anything is assigned.
