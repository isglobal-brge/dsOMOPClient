# Create a safe population filter for a numeric measurement interval

Requests the controller-configured public grid for one measurement
concept, then snaps the requested closed-open interval outwards to edges
issued by every connected site. The result is executable as a
population-level `has_measurement` filter. Exact or one-sided
client-authored thresholds are deliberately not supported.

## Usage

``` r
ds.omop.safe.filter.measurement(
  concept_id,
  min_value,
  max_value,
  n_bins = 10L,
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- concept_id:

  One measurement concept ID.

- min_value, max_value:

  Finite requested interval limits. Both are required and must lie
  inside the common public grid.

- n_bins:

  Integer; the number of bins for cutpoint computation (default: 10).

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

## Value

An authenticated population-level `omop_filter`.
