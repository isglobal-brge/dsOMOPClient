### 4.17 `survival.first_event`

Parameters: common; `outcome: event` required; `start: integer = 1`, at least
0; `horizon: integer = 365`, at least `start`; `prior_days: integer = 365`,
at least 0. Entry is TAR start. `prior_days=0` denotes an empty prior window,
not all-time exclusion. All dates are relative to the selected index.

```r
outcome <- omop_param("outcome", "event")
start <- omop_param("start", "integer", default = 1L)
horizon <- omop_param("horizon", "integer", default = 365L)
prior_days <- omop_param("prior_days", "integer", default = 365L)
outputs <- list(omop_output("survival", rows = "study", format = "survival",
  grain = "episode", options = list(outcomes = list(outcome = outcome),
    tar = omop_window(start, horizon), format = "survival", event_order = "first",
    censor = list("observation_end", "death"),
    prior_outcome = list(lookback = prior_days, action = "exclude"),
    tie_policy = "priority", same_day_priority = "event")))
publish <- "survival"
```

Output: `survival`, schema `S`. `outcome_name="outcome"` on event and censored
rows. All-cause mortality is expressible by binding an unrestricted death
event; an outcome on the death censor date wins with the stated priority.

Documentation: “Assign elapsed time to the first qualifying event during the
inclusive risk window, capped by death and observation end. Prior outcomes
are excluded relative to effective entry. Single-row follow-up stops on the
last observed day before a gap; events in unobserved gaps do not count.
Zero elapsed days is possible when entry and exit coincide.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
