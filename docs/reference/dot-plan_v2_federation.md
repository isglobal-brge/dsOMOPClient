# Bind (or re-check) the multi-server contract of a v2 plan

The contract binds the plan's semantic digest (section 4.4: the
federation harmonisation signature and the DP provenance capsule bind
the same digest), the participating servers and the schema snapshot of
every table the plan needs. It is stored as the plan's `"harmonization"`
attribute, never inside the wire plan (whose schema is closed).

## Usage

``` r
.plan_v2_federation(plan, symbol, conns)
```
