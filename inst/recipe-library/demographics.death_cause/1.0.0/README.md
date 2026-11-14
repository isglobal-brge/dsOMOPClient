### 4.5 `demographics.death_cause`

Parameters: common; `horizon: integer = 365`, at least 0. Requires `death` with
`death_date` and `cause_concept_id`. The reporting window includes index day.

```r
events <- list()
intervals <- list()
variables <- list()
index <- omop_param("index", "event", description = "Event or era defining entry")
select <- omop_param("select", "string", default = "first_eligible",
                     description = "Selection among index candidates")
populations <- list(omop_population("study",
  index = omop_index(index, select = select,
                     end = omop_end("observation_end")),
  coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                           on_missing = "drop", gap = 0L)))

horizon <- omop_param("horizon", "integer", default = 365L)
death <- omop_event("death", name = "death")
events <- list(death)
variables <- list(
  death_recorded = omop_variable(event = death,
    reducer = omop_reduce("binary", window = omop_window(0L, horizon))),
  death_days = omop_variable(event = death,
    reducer = omop_reduce("first_date", window = omop_window(0L, horizon))),
  cause_concept_id = omop_variable(event = death,
    reducer = omop_reduce("first_value", value = "concept_id", window = omop_window(0L, horizon),
                          missing = "keep")))
outputs <- list(omop_output("death_cause", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "death_cause"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
```

Output: `death_cause`, `K + death_recorded, death_days, cause_concept_id`.
`first_date` and `first_value(missing="keep")` select the same first death
row, so an unknown cause cannot shift the cause to another row. Missing row:
0/NA/NA; cause 0 remains 0. This table is recorded-death description, not an
observed-risk survival analysis; reducers do not censor records at observation
end. Use §4.17–19 for follow-up definitions.

Documentation: “Assign whether death was recorded from index through the
horizon, its first recorded day offset, and that row's coded cause. Missing
or unspecified cause is not evidence of a different cause. No cause-of-death
hierarchy or underlying-versus-contributing cause is inferred.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.

Integrated API correction: coded concepts are selected by the reducer value role (`value_as_concept_id`, or the death source's `concept_id`), not the event numeric `value` override. The latter rejects concept columns in 2.10.4. The shipped `definition.R` is authoritative; row selection, missingness and tie policies are unchanged.
