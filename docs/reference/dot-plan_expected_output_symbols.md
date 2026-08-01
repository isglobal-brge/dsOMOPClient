# Derive the exact server symbols owned by a plan execution

Composite OHDSI-style outputs are split by the server into a fixed set
of symbols. Deriving that set from the requested output type/format
avoids treating stale, merely prefix-matching symbols as part of the
current run.

## Usage

``` r
.plan_expected_output_symbols(plan, out)
```

## Arguments

- plan:

  An `omop_plan`.

- out:

  Named output-to-symbol mapping.

## Value

Named list, one exact character vector per requested output.
