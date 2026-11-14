### 4.14 `cohort.new_users`

Replaces the common population. Parameters: `drug: concept_set` required;
`washout_days: integer = 365`, at least 0; `select: string =
"first_eligible"` (common choices); `by: string = "ingredient"`, one of
`ingredient`, `concept`, `event`. No separate `index` parameter.

```r
drug <- omop_param("drug", "concept_set")
washout <- omop_param("washout_days", "integer", default = 365L)
select <- omop_param("select", "string", default = "first_eligible",
                     description = "Selection among index candidates")
by <- omop_param("by", "string", default = "ingredient")
nu <- omop_new_users(drug, washout_days = washout, select = select, by = by)
populations <- list(nu)
variables <- list(age = omop_age(), sex = omop_sex(),
                   index_source = omop_population_column("index_source"))
outputs <- list(omop_output("new_users", rows = "new_users", format = "wide",
  grain = "episode", columns = names(variables),
  options = list(date_handling = "relative")))
publish <- "new_users"
```

Output: `new_users`, `K + age, sex, index_source`. Helper expansion is exactly
A-23: era gap 30, persistence 0, `min_days=0`, recorded ends with supply
fallback, no stockpiling, own-concept fallback for unmapped ingredients,
interval-end episode end. Positive washout adds observed-before and absence
of drug **starts** in `[-washout,-1]`; zero adds neither. Binding zero must
not construct a reversed window. It does not exclude every possible exposure
whose start precedes the washout but whose supply overlaps it; see G5.

Documentation: “Assign a new-user episode roster using the recipe language's
standard drug-era and washout definition. First-eligible selection can retain
a later eligible course after an earlier ineligible candidate. Ingredient
mapping and same-day candidate order are deterministic. A zero washout means
no washout or prior-observation requirement, not a claim of incident use.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
