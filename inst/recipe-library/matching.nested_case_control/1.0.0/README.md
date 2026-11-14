### 4.24 `matching.nested_case_control`

Replaces the common population. Parameters: `outcome: event` required and
containing the complete relevant history; `exposure: event` required;
`lookback: integer = 365`, at least 1; `controls_per_case: integer = 4`, at
least 1; `age_width: integer = 5`, at least 1; `seed: integer = 1`, at least
0. Fixed matching on sex and annual-age band; case-time alignment already
sets calendar time. No arbitrary variable/model parameter is accepted.

```r
outcome <- omop_param("outcome", "event")
exposure <- omop_param("exposure", "event")
lookback <- omop_param("lookback", "integer", default = 365L)
m <- omop_param("controls_per_case", "integer", default = 4L)
age_width <- omop_param("age_width", "integer", default = 5L)
seed <- omop_param("seed", "integer", default = 1L)
populations <- list(
  omop_population("cases",
    index = omop_index(outcome, select = "first_eligible",
                       end = omop_end("fixed", days = 0L)),
    coverage = omop_coverage(), filters = list(
      omop_observed(before = lookback),
      omop_has_not(outcome, window = omop_window(NULL, -1L)))),
  omop_population("controls", coverage = omop_coverage(),
    filters = list(omop_observed(before = lookback))),
  omop_population("matched", match = omop_match(cases = "cases", controls = "controls",
    outcome = outcome, at = "index", match = list("sex", omop_age_band(age_width)),
    controls_per_case = m, prior_outcome = "exclude", future_cases = "eligible",
    replacement = "between_sets", seed = seed, insufficient = "keep_available",
    missing_strata = "ineligible", max_candidates = 1000000L)))
variables <- list(set_id = omop_population_column("set_id"),
  is_case = omop_population_column("is_case"),
  sampling_weight = omop_population_column("sampling_weight"),
  age = omop_age(), sex = omop_sex(),
  exposure_history = omop_variable(event = exposure,
    reducer = omop_reduce("binary", window = omop_window(-lookback, -1L))))
outputs <- list(omop_output("matched", rows = "matched", format = "wide",
  grain = "episode", columns = names(variables),
  options = list(date_handling = "relative")))
publish <- "matched"
```

Output: `matched`, `K + set_id, is_case, sampling_weight, age, sex,
exposure_history`. Cases have weight 1; a sampled control has eligible-pool
size divided by sampled controls in that case set. The weight encodes that
pool information; it is assigned data under the existing support rule, not a
DP guarantee. A case with no controls is retained. Reused controls have
distinct episode identities across sets. `set_id` is a protected synthetic
analysis key, never a person count or a cross-execution join promise.

Documentation: “Assign deterministic risk-set samples at first eligible
recorded case times, matching sex and annual-age bands. Controls are observed
and alive at the case time, have no outcome at or before that time, and may
become future cases or be reused between sets. The controls criteria are
reevaluated at each case time. Incomplete sets are retained. This is nested
case-control sampling, not propensity-score or general nearest-neighbour
matching.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
