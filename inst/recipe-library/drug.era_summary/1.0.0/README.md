### 4.12 `drug.era_summary`

Parameters: common; `drug: concept_set` required; `gap: integer = 30`, at
least 0; `persistence: integer = 0`, at least 0; `window: window =
omop_window(0,365)`. Fixed `by="event"` treats the declared drug set as one
exposure class. This choice differs deliberately from the ingredient default
of `omop_new_users()`.

```r
drug <- omop_param("drug", "concept_set")
gap <- omop_param("gap", "integer", default = 30L)
persistence <- omop_param("persistence", "integer", default = 0L)
window <- omop_param("window", "window", default = omop_window(0L, 365L))
rx <- omop_event("drug_exposure", drug, end = list(fallback = "days_supply"),
                 name = "rx")
course <- omop_era(rx, gap = gap, persistence = persistence, min_days = 0L,
                   by = "event", end = "recorded_end", fallback = "days_supply",
                   stockpile = FALSE, name = "course")
events <- list(rx)
intervals <- list(course)
variables <- list(
  era_count = omop_variable(event = course,
    reducer = omop_reduce("era_count", window = window)),
  era_days = omop_variable(event = course,
    reducer = omop_reduce("era_days", window = window)),
  first_era_days = omop_variable(event = course,
    reducer = omop_reduce("first_era_days", window = window)),
  covered_days = omop_variable(event = course,
    reducer = omop_reduce("covered_days", window = window, coverage = "recorded")))
outputs <- list(omop_output("eras", rows = "study", format = "wide", grain = "episode", columns = names(variables), options = list(date_handling = "relative")))
publish <- "eras"
```

Output: `eras`, `K + era_count, era_days, first_era_days, covered_days`.
Era reducers retain their existing window-selection/course-duration semantics;
only `covered_days` is the explicitly window-clipped union. Course envelope
days must not be described as medication-covered days. Empty source gives
0/NA/NA/0 respectively; undefined window or unavailable source gives NA.

Documentation: “Summarise exposure courses for one declared drug class.
Courses merge nearby fills using the stated gap and persistence; recorded
covered days union actual recorded intervals and exclude those extensions.
Coverage here is not restricted to observed days. Missing recorded ends use
valid days supply, then the language's point-record policy.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
