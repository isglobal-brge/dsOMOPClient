# Legacy temporal-covariates and person-period presets lowered to v2

Sources are the groups of variables reading the same rows, split by the
analyses each concept requests (`count` for count variables, `binary`
otherwise); the output's analyses are the requested ones unless
`options$analyses` names them. A source whose analyses differ from the
output's carries them in `source_analyses` (A-14), so only the declared
`(source, concept, analysis)` pairs are computed, each a covariate
identity of the server's registry (design 6.12).

## Usage

``` r
.v2_legacy_temporal_options(ctx, out, vars, context, format)
```
