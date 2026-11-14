### 4.2 `socioeconomic.education`

Parameters: common; `education: concept_set` required (observation question
concepts); `lookback: integer = 365`, at least 1. Requires `observation` with
`value_as_concept_id`. Answers are categorical OMOP concepts, not free text.

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

education <- omop_param("education", "concept_set")
lookback <- omop_param("lookback", "integer", default = 365L)
edu <- omop_event("observation", education,
                  name = "education")
events <- list(edu)
variables <- list(
  education_concept_id = omop_variable(event = edu,
    reducer = omop_reduce("last_value", value = "value_as_concept_id", window = omop_window(-lookback, -1L),
                          missing = "keep")),
  education_records = omop_variable(event = edu,
    reducer = omop_reduce("count", window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("education", rows = "study", format = "wide", grain = "episode",
  columns = names(variables), options = list(date_handling = "relative")))
publish <- "education"
omop_recipe(events = events, intervals = intervals,
  populations = populations, variables = variables,
  outputs = outputs, publish = publish)
```

Output: `education`, `K + education_concept_id, education_records`.
The selected concept value is numeric/lossless concept data; 0 stays recorded
concept 0, never recoded to a level. Count includes rows with missing answers.
Equal dates use the language's total event order.

Documentation: “Assign the last recorded coded education answer before index
and the number of education records in the lookback window. A missing last
answer stays missing rather than being replaced by an older answer. This
does not infer highest educational attainment or order education concepts.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.

Integrated API correction: coded concepts are selected by the reducer value role (`value_as_concept_id`, or the death source's `concept_id`), not the event numeric `value` override. The latter rejects concept columns in 2.10.4. The shipped `definition.R` is authoritative; row selection, missingness and tie policies are unchanged.
