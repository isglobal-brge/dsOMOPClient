### 4.1 `demographics.baseline`

Parameters: common `index` and `select`; `lookback: integer = 365`, at least 1.
Requires person and observation-period data and the index source.

```r
lookback <- omop_param("lookback", "integer", default = 365L)
variables <- list(
  age = omop_age(reference = "index", bands = FALSE),
  sex = omop_sex(), race = omop_race(), ethnicity = omop_ethnicity(),
  prior_observed_days = omop_observed_days(omop_window(-lookback, -1L)))
outputs <- list(omop_output("baseline", rows = "study", format = "wide", grain = "episode", columns = names(variables), options = list(date_handling = "relative")))
publish <- "baseline"
```

Output: `baseline`, `K + age, sex, race, ethnicity, prior_observed_days`.
Age is annual integer age; sex is the existing M/F/unknown mapping; race and
ethnicity retain the existing helper representation. Birth date is not emitted.

Documentation: “Assign one baseline row per selected index episode, containing
annual age, recorded demographic categories and the number of observed days
in the preceding lookback window. Missing demographic information stays
missing; a short observed history does not by itself exclude the episode.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
