### 4.23 `longitudinal.event_log`

Parameters: common; `source_a`, `source_b: event` required. Both must have
time roles; provide appropriately restricted/named event definitions. Fixed
common-schema event log, relative dates, episode-event association. There is
no report-window parameter: all associated source rows permitted by the
native long-output association are requested. Narrow source predicates or
edit the recipe when a finite reporting interval is needed.

```r
source_a <- omop_param("source_a", "event")
source_b <- omop_param("source_b", "event")
outputs <- list(omop_output("event_log", rows = "study", format = "long",
  grain = "episode", sources = list(source_a, source_b),
  options = list(event_log = TRUE, unit = "episode_event", date_handling = "relative")))
publish <- "event_log"
```

Output: `event_log`, schema `EL`. `source` is the resolved source namespace,
not assumed to be the parameter name. Missing roles have the language's
typed missing values. This is an explicit event log, not legacy split `long`;
event-free episodes have no fabricated event rows and final event-log support
can consequently be smaller than the index roster.

Documentation: “Assign a common-schema timeline from two declared sources,
preserving source, concept, relative interval and numeric value/unit identity.
One physical event can be associated with multiple index episodes. A source
label identifies provenance, not a common unit or comparable measurement.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
