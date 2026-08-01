# Build the common disclosure/harmonisation contract for a federation

The contract is deliberately a coarsening contract, not a "first server
wins" rule. Age boundaries are intersected across every node, minimum
age and date windows take the largest (most restrictive) value, and
count bands must be identical because sums of differently rounded site
counts do not have one documented release granularity.

## Usage

``` r
.federated_harmonization_contract(
  settings,
  expected_servers = names(settings),
  fail = TRUE
)
```

## Arguments

- settings:

  Named list of per-server disclosure settings.

- expected_servers:

  Character vector of servers that must be represented.

- fail:

  Logical; stop when no common semantic/age contract exists.

## Value

A named harmonisation-contract list.
