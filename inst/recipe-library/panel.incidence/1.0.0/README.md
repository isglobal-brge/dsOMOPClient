### 4.21 `panel.incidence`

Parameters: common; `outcome: event` required; `horizon: integer = 365`, at
least 0. Fixed calendar-year bins and `[0,horizon]` reporting window.

```r
outcome <- omop_param("outcome", "event")
horizon <- omop_param("horizon", "integer", default = 365L)
outputs <- list(omop_output("incidence", rows = "study", format = "person_period",
  grain = "episode", options = list(representation = "dense",
    bins = list(calendar = "year"), window = omop_window(0L, horizon),
    outcomes = list(outcome = outcome))))
publish <- "incidence"
```

Output: `incidence`, `PP + n_outcome` (integer count). No raw events are needed
to seed a period. Person time comes from the selected observed component.
Counting repeated events and overlapping episodes is intentional under this
generic panel; there is no first-event risk-set removal or automatic
person-time deduplication across episodes.

Documentation: “Assign calendar-year person-period denominators and counts
of recorded outcomes within observed periods. Zero-event periods remain.
This supplies event-incidence components; it is not by itself a first-disease
incidence rate. Choose an appropriate roster and event definition before
aggregating, and account explicitly for overlapping episodes.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
