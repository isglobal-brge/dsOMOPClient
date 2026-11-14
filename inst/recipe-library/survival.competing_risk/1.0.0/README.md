### 4.18 `survival.competing_risk`

Parameters: common; `outcome_a`, `outcome_b: event` required; `horizon:
integer = 365`, at least 1. Fixed start day 1, prior-outcome exclusion 365
days, cause priority A then B. Two named causes are chosen to avoid inventing
a list-valued parameter; a larger cause set is an ordinary recipe edit.

```r
outcome_a <- omop_param("outcome_a", "event")
outcome_b <- omop_param("outcome_b", "event")
horizon <- omop_param("horizon", "integer", default = 365L)
outputs <- list(omop_output("competing_risk", rows = "study", format = "survival",
  grain = "episode", options = list(outcomes = list(cause_a = outcome_a, cause_b = outcome_b),
    tar = omop_window(1L, horizon), format = "competing_risk", event_order = "first",
    censor = list("observation_end", "death"),
    prior_outcome = list(lookback = 365L, action = "exclude"),
    tie_policy = "priority", same_day_priority = "event")))
publish <- "competing_risk"
```

Output: `competing_risk`, schema `S`; `outcome_name` is selected `cause_a` or
`cause_b`, NULL if censored. The native `event` field is the format's event
indicator; cause identity is read from `outcome_name`, not invented factor
codes. If death is not bound as a cause, it is censoring, not an automatically
added competing outcome.

Documentation: “Assign the first of two declared outcomes, retaining the
selected cause. Same-day causes follow declared A-before-B priority. Later
causes are not retained in this format. To model death as a competing event,
supply it explicitly as one of the outcome sources.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
