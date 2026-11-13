# Validate a server-owned analysis pooling contract

Validate a server-owned analysis pooling contract

## Usage

``` r
.validate_analysis_pooling_contract(contract, per_site = NULL)
```

## Arguments

  - contract:

    Contract returned by omopAnalysisGetDS().

  - per\_site:

    Optional named list of returned data frames. When supplied, each
    frame must have exactly the contracted schema, including empty
    frames.

## Value

The validated contract, invisibly.
