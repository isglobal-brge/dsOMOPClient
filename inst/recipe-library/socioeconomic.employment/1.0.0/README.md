### 4.3 `socioeconomic.employment`

Parameters: common; `employment: concept_set` required (observation question
concepts); `lookback: integer = 365`, at least 1. Requires coded observations.
This is a distinct immutable entry, not an alias to education.

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

employment <- omop_param("employment", "concept_set")
lookback <- omop_param("lookback", "integer", default = 365L)
job <- omop_event("observation", employment,
                  name = "employment")
events <- list(job)
variables <- list(
  employment_concept_id = omop_variable(event = job,
    reducer = omop_reduce("last_value", value = "value_as_concept_id", window = omop_window(-lookback, -1L),
                          missing = "keep")),
  employment_records = omop_variable(event = job,
    reducer = omop_reduce("count", window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("employment", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "employment"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
```

Output: `employment`, `K + employment_concept_id, employment_records`.
Missingness, concept-0 and tie rules are the same as education.

Documentation: “Assign the last coded employment-status answer before index
and its source-record count. Occupation, employment status and income are
different constructs: supply the intended question concepts. The recipe does
not harmonise free text or assume an ordering of occupations.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.

Integrated API correction: coded concepts are selected by the reducer value role (`value_as_concept_id`, or the death source's `concept_id`), not the event numeric `value` override. The latter rejects concept columns in 2.10.4. The shipped `definition.R` is authoritative; row selection, missingness and tie policies are unchanged.
