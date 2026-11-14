### 4.20 `panel.measurements`

Parameters: common; `measure: event` required; `horizon: integer = 365`, at
least 0; `bin_days: integer = 30`, at least 1; `lookback: integer = 90`, at
least 0; `expire: integer = 30`, at least 0. Require a named measurement
event `measure` with an explicitly defined finite concept set and numeric
unit policy. Parameter bindings may carry that event name; alternatively use
the declared source reference with the event added through the ordinary API.
Name/role validation happens before execution; arbitrary events are not
silently renamed by the library.

```r
measure <- omop_param("measure", "event")
horizon <- omop_param("horizon", "integer", default = 365L)
bin_days <- omop_param("bin_days", "integer", default = 30L)
lookback <- omop_param("lookback", "integer", default = 90L)
expire <- omop_param("expire", "integer", default = 30L)
outputs <- list(omop_output("measurements", rows = "study", format = "person_period",
  grain = "episode", options = list(representation = "dense", sources = list(measure),
    bins = list(width = bin_days), window = omop_window(0L, horizon),
    analyses = c("count", "mean", "last"),
    carry_forward = list(measure = list(lookback = lookback, expire = expire)))))
publish <- "measurements"
```

Output: `measurements`, `PP` plus `measure_count`, `measure_mean`,
`measure_last`, `measure_carry` when the resolved plan/vocabulary has exactly
one concept identity; otherwise `measure_<concept_id>_<analysis>` and
`measure_<concept_id>_carry` for each declared identity (A-3). Columns cannot
depend on which clinical rows are present at a site. A finite multi-concept
binding does not pool different analytes. Duplicate/ambiguous source names
are errors, not silently repaired. No numeric mean is meaningful across
incompatible units; the event must restrict or normalise them explicitly.

Documentation: “Assign dense observed periods with measurement count, mean,
last value and a separately named carry-forward value at period start.
Carry-forward uses the last nonmissing value at or before that start within
lookback and expiry. It does not replace period mean/last, use future values,
or assert that a value remains constant throughout the period. Event-free
periods retain count zero and numeric NA; an unavailable source gives NA.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.

Binding requires an explicit scale/unit policy: use `omop_unit(target, convert = ..., unknown = "missing" or "exclude")`, or an `omop_where()` predicate restricting `unit_concept_id` to exactly one declared unit. A unitless scale can explicitly declare unit 0. Conversions and scales are study definitions; neither site rows nor concept names establish comparability. The same rule applies after binding source references, import and prefixed composition.
