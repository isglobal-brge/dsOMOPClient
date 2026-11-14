### 4.4 `socioeconomic.numeric_observation`

Parameters: common; `measure: event` required (numeric observation/measurement
source with a single declared scale/unit policy); `lookback: integer = 365`,
at least 1. The analyst supplies an event with a numeric value role and any
explicit `omop_unit()` conversions; no currency or scale conversion is inferred.

```r
measure <- omop_param("measure", "event")
lookback <- omop_param("lookback", "integer", default = 365L)
variables <- list(
  numeric_value = omop_variable(event = measure,
    reducer = omop_reduce("last_value", window = omop_window(-lookback, -1L),
                          missing = "keep")),
  source_records = omop_variable(event = measure,
    reducer = omop_reduce("count", window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("numeric_observation", rows = "study", format = "wide", grain = "episode", columns = names(variables), options = list(date_handling = "relative")))
publish <- "numeric_observation"
```

Output: `numeric_observation`, `K + numeric_value, source_records`.
Documentation: “Assign the latest pre-index value of a declared numeric social
measure, such as a recorded deprivation score, on its supplied scale. This
extracts recorded observations; it does not derive an area score from an
address, equate different instruments or convert currencies.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.

Binding requires an explicit scale/unit policy: use `omop_unit(target, convert = ..., unknown = "missing" or "exclude")`, or an `omop_where()` predicate restricting `unit_concept_id` to exactly one declared unit. A unitless scale can explicitly declare unit 0. Conversions and scales are study definitions; neither site rows nor concept names establish comparability. The same rule applies after binding source references, import and prefixed composition.
