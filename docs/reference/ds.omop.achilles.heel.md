# Get Achilles Heel data-quality warnings (per site)

Achilles Heel counts records and does not provide distinct-person
support for each fired rule. It therefore cannot satisfy dsOMOP's
person-level contribution contract and is no longer available through
DataSHIELD. Run it only in a controller-side quality-assurance workflow.

## Usage

``` r
ds.omop.achilles.heel(symbol = "omop", conns = NULL)
```

## Arguments

- symbol:

  Character; session symbol (default "omop").

- conns:

  Optional DataSHIELD connections.

## Value

A `dsomop_result` with per-site heel warning data frames.
