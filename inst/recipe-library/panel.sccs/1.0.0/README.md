### 4.22 `panel.sccs`

Parameters: common; `drug: concept_set` required; `outcome: event` required;
`horizon: integer = 365`, at least 0; `age_width: integer = 5`, at least 1.
Fixed class exposure eras with gap=0/persistence=0 and no stockpile. The
template is an SCCS **design table**, not an estimator or a guarantee of the
assumptions of any particular SCCS model.

```r
drug <- omop_param("drug", "concept_set")
outcome <- omop_param("outcome", "event")
horizon <- omop_param("horizon", "integer", default = 365L)
age_width <- omop_param("age_width", "integer", default = 5L)
rx <- omop_event("drug_exposure", drug, name = "rx")
exposure <- omop_era(rx, gap = 0L, persistence = 0L, by = "event",
                     end = "recorded_end", fallback = "days_supply", name = "exposure")
events <- list(rx)
intervals <- list(exposure)
outputs <- list(omop_output("sccs", rows = "study", format = "person_period",
  grain = "episode", options = list(representation = "dense",
    bins = list(calendar = "year"), window = omop_window(0L, horizon),
    partition = list(boundaries = list(exposure), age_bands = age_width, calendar = "year"),
    exposures = list(drug = exposure), outcomes = list(outcome = outcome))))
publish <- "sccs"
```

Output: `sccs`, `PP + exposed_drug, active_drug, n_outcome`.
The boundary set is seeded with window start/end+1 and split at observation,
exposure, calendar and annual-age boundaries. Exposed days use interval union,
not a sum of overlapping fills. Annual age changes on 1 January. It includes
all selected episodes; case-only selection must be an explicit population
edit if the analysis requires it, not a hidden post-assignment subset.

Documentation: “Assign observed intervals partitioned by drug exposure,
annual-age bands and calendar years, with outcome counts and observed-day
denominators. This simple risk definition is exposure time itself. Lagged,
pre-exposure and post-exposure risk categories are not automatically added,
and the recipe performs no conditional Poisson fitting.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
