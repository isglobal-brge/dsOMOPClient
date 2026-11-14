### 4.11 `comorbidity.condition_history`

Parameters: common; `conditions: concept_set` required;
`lookback: integer = 365`, at least 1.

```r
conditions <- omop_param("conditions", "concept_set")
lookback <- omop_param("lookback", "integer", default = 365L)
dx <- omop_event("condition_occurrence", conditions, name = "history")
events <- list(dx)
variables <- list(
  history_present = omop_variable(event = dx,
    reducer = omop_reduce("binary", window = omop_window(-lookback, -1L))),
  history_days = omop_variable(event = dx,
    reducer = omop_reduce("count", distinct = "day",
                          window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("history", rows = "study", format = "wide", grain = "episode", columns = names(variables), options = list(date_handling = "relative")))
publish <- "history"
```

Output: `history`, `K + history_present, history_days` (integers).
Documentation: “Assign any history and the number of distinct recorded days
for a declared condition set before index. Multiple records on one day count
once. This is a covariate, not a validated multi-category comorbidity score.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
