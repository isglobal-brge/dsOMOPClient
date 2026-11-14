### 4.19 `survival.recurrent_events`

Parameters: common; `outcome: event` required; `horizon: integer = 365`, at
least 1; `washout_days: integer = 0`, at least 0 (event separation, not the
cohort's drug washout). No prior-outcome exclusion; all retained events in
the observed follow-up are requested. Fixed `complete=TRUE`.

```r
outcome <- omop_param("outcome", "event")
horizon <- omop_param("horizon", "integer", default = 365L)
washout <- omop_param("washout_days", "integer", default = 0L)
outputs <- list(omop_output("recurrent", rows = "study", format = "survival",
  grain = "episode", options = list(outcomes = list(outcome = outcome),
    tar = omop_window(1L, horizon), format = "recurrent_events", event_order = "all",
    washout_days = washout, censor = list("observation_end", "death"),
    tie_policy = "priority", same_day_priority = "event", complete = TRUE)))
publish <- "recurrent"
```

Outputs: `recurrent.events` (`RE`) and `recurrent.risk_sets` (`RR`). An eligible
event-free episode has the A-5 event=0 sentinel with event_number=1 and NULL
outcome-specific fields; an episode with exit before entry yields neither
component. Sentinels are not diagnoses. Each patient component has its own
genuine-person support check; a larger risk-set frame never licenses a
smaller events component.

Documentation: “Assign recurrent event rows and their risk-set intervals,
including explicit event-free episodes. Event separation and same-day
priority use the existing recurrent-event rules. This recipe does not fit a
recurrent-event model or interpret event numbers as independent persons.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
