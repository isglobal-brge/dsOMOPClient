# Legacy survival preset lowered to the language-2 survival format

Outcomes are the output's concept variables (one composite outcome over
their concepts, or one named endpoint per variable); the TAR keeps the
legacy default of 0 to 730 days after index (F-4); censoring keeps the
legacy rules: the end of the observation period containing the index
(unless `censoring$observation_period_end = FALSE`), death when
`censoring$death = TRUE`, an administrative date when given, and the
mandatory cohort-end cap as `"index_end"`.

## Usage

``` r
.v2_legacy_survival_options(ctx, out, vars, context)
```
