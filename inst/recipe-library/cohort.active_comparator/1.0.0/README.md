### 4.15 `cohort.active_comparator`

Replaces the common population. Parameters: `drug_a`, `drug_b: concept_set`
required; `outcome: event` required (complete outcome history);
`washout_days: integer = 365`, at least 1; `horizon: integer = 365`, at least
1; `grace_days: integer = 30`, at least 0. Selection is fixed first-eligible
across the two ordered exposure-class sources, A then B. No independent
per-arm first-user selections are silently combined.

```r
drug_a <- omop_param("drug_a", "concept_set")
drug_b <- omop_param("drug_b", "concept_set")
outcome <- omop_param("outcome", "event")
washout <- omop_param("washout_days", "integer", default = 365L)
horizon <- omop_param("horizon", "integer", default = 365L)
grace <- omop_param("grace_days", "integer", default = 30L)
a <- omop_event("drug_exposure", drug_a, name = "rx_a")
b <- omop_event("drug_exposure", drug_b, name = "rx_b")
ea <- omop_era(a, by = "event", gap = 30L, persistence = 0L,
               end = "recorded_end", fallback = "days_supply", name = "era_a")
eb <- omop_era(b, by = "event", gap = 30L, persistence = 0L,
               end = "recorded_end", fallback = "days_supply", name = "era_b")
events <- list(a, b)
intervals <- list(ea, eb)
populations <- list(omop_population("study",
  index = omop_index(omop_any_of(ea, eb, order = c("era_a", "era_b")),
    select = "first_eligible", end = omop_end("interval_end")),
  coverage = omop_coverage(), filters = list(
    omop_observed(before = washout),
    omop_has_not(a, window = omop_window(-washout, -1L)),
    omop_has_not(b, window = omop_window(-washout, -1L)))))
variables <- list(arm = omop_population_column("index_source"),
                   age = omop_age(), sex = omop_sex())
outputs <- list(
  omop_output("baseline", rows = "study", format = "wide", grain = "episode", columns = names(variables), options = list(date_handling = "relative")),
  omop_output("itt", rows = "study", format = "survival", grain = "episode",
    options = list(outcomes = list(outcome = outcome),
      tar = omop_window(1L, horizon), format = "survival", event_order = "first",
      prior_outcome = list(lookback = 365L, action = "exclude"),
      censor = list("observation_end", "death"), same_day_priority = "event")),
  omop_output("on_treatment", rows = "study", format = "survival", grain = "episode",
    options = list(outcomes = list(outcome = outcome),
      tar = omop_window(1L, horizon), format = "survival", event_order = "first",
      prior_outcome = list(lookback = 365L, action = "exclude"),
      censor = list("observation_end", "death",
        omop_censor(ea, which = "containing_entry_end", offset = grace,
                    missing = "ineligible", when_index_source = "era_a"),
        omop_censor(eb, which = "containing_entry_end", offset = grace,
                    missing = "ineligible", when_index_source = "era_b"),
        omop_censor(eb, which = "first_start_after_entry", when_index_source = "era_a"),
        omop_censor(ea, which = "first_start_after_entry", when_index_source = "era_b")),
      same_day_priority = "event")),
  omop_output("itt_analysis", rows = "study", format = "joined", grain = "episode",
    options = list(from = list(list(output = "itt"), list(output = "baseline")),
                    how = "left", cardinality = "many_to_one")),
  omop_output("on_treatment_analysis", rows = "study", format = "joined", grain = "episode",
    options = list(from = list(list(output = "on_treatment"), list(output = "baseline")),
                    how = "left", cardinality = "many_to_one")))
publish <- c("itt_analysis", "on_treatment_analysis")
```

Outputs: `itt_analysis` and `on_treatment_analysis`, each `S + arm, age, sex`
(shared identity keys occur once under the existing joined-output contract).
`arm` is the source tag `era_a`/`era_b`, with that public mapping in metadata.
Baseline and survival inputs are private; only these joined tables are roots.
No separate arm support gate is introduced. Outcome-specific eligibility can
make their rosters smaller than the cohort, and on-treatment eligibility can
make the two final tables differ.

The on-treatment definition above requires the index-arm course to contain
effective entry (day 1). Its end plus grace caps follow-up, with administrative
horizon, observation/death and opposite-arm switch caps. A course ending on
index day does **not** contain entry and is ineligible for this on-treatment
definition even with grace. It must not silently attach a later own-arm course.
For the alternative index-course-end-plus-grace definition including such
episodes, the existing language can instead use a TAR ending at
`omop_bound("index_end", grace)` with a separately specified administrative
censor; that is a different template version/edited analysis.

Both washouts cover both arm sources. Same-day dual candidates resolve to A
by the declared source order. Drug sets should represent the study's intended
alternative treatments; the library does not query their patient-level
overlap, discard same-day dual users or claim causal exchangeability. An
opposite-arm course already active before day 1 is not a new switch after
entry; the declared first-start rule is retained, not rewritten silently.
Same-day outcome/censor priority is event. Prior-outcome lookback is fixed 365
days relative to effective entry and includes index day, independent of drug
washout. No future-observation eligibility criterion is imposed.

Documentation: “Assign baseline-adjusted analysis tables for first eligible
initiation of either of two exposure classes. ITT follows the selected
initiation; on-treatment follow-up additionally requires an active index-arm
course at entry and censors at its end plus grace or the first opposite-arm
course starting from entry. The tables carry the declared treatment source.
These are study design tables; the recipe performs no confounding adjustment.”



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
