### 4.16 `outcome.incident_event`

Replaces the common population. Parameters: `outcome: event` required (full
event history); `lookback: integer = 365`, at least 1. Fixed first-eligible
event index, point-ended episodes, observation and absence requirements.

```r
outcome <- omop_param("outcome", "event")
lookback <- omop_param("lookback", "integer", default = 365L)
populations <- list(omop_population("study",
  index = omop_index(outcome, select = "first_eligible",
                     end = omop_end("fixed", days = 0L)),
  coverage = omop_coverage(), filters = list(
    omop_observed(before = lookback),
    omop_has_not(outcome, window = omop_window(-lookback, -1L)))))
variables <- list(age = omop_age(), sex = omop_sex(),
  prior_observed_days = omop_observed_days(omop_window(-lookback, -1L)))
outputs <- list(omop_output("incident_events", rows = "study", format = "wide", grain = "episode", columns = names(variables), options = list(date_handling = "relative")))
publish <- "incident_events"
```

Output: `incident_events`, `K + age, sex, prior_observed_days`.
Documentation: “Assign first eligible events after a fully observed,
event-free lookback. Incident means no qualifying recorded event in that
lookback, not absence of disease throughout life. Same-day duplicates follow
the deterministic index-event order. This is a case roster, not an incidence
rate denominator.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
