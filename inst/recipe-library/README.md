# Installed recipe library

The 24 versioned templates are local client definitions for the 2.10.4 native
recipe language. They assign analysis tables through the ordinary
`recipe_execute()` path. They are distinct from the aggregate QueryLibrary.

```r
recipe_library_list(category = c("demographics", "survival"))
recipe_library_describe("demographics.baseline", version = "1.0.0")
r <- recipe_library_instantiate("demographics.baseline", version = "1.0.0")
r <- recipe_bind(r, index = omop_event("visit_occurrence", 9201))
recipe_to_code(r)
recipe_to_plan(r, version = 2L)
# Connected execution, explicitly requested by the analyst:
# recipe_execute(r, out = c(baseline = "analysis_baseline"), conns = conns)
```

Each ID/version directory contains its exact constructor expansion,
parameter/default documentation, fixed policies, output schema and grain,
clinical interpretation, limitations and fixture reference. `describe()`
returns these together with the expanded ordinary recipe. Required values
remain unbound (`default = NULL`); supply concept sets and event definitions
as study inputs. Numeric IDs are exact sets; use `omop_concept_set()` for
explicit descendants, mappings and exclusions. No site data choose concepts,
units, cutpoints, columns or thresholds. Empty sets never mean all concepts.

Common index selection defaults to `first_eligible`; allowed selections are
`first`, `first_eligible`, `last`, `all`. Coverage is unique/drop/drop/gap=0.
An ambiguous or uncovered index is privately dropped. All windows are inclusive
whole days; baseline windows end on day -1. No full baseline observation is
required unless the population explicitly says so. Annual age is index year
minus birth year. Outputs have episode grain, including multiple episodes
when selection is `all`; no hidden person collapse is performed.

Available event-free count/binary sources yield zero; unavailable optional
sources yield NA. Numeric missingness and ties follow the declared reducers.
Tables are required unless an analyst explicitly supplies an event with
`on_missing_table = "unavailable"`. Coded values retain the native concept
representation/presentation. Patient keys use existing publication protection.
Dense measurement columns depend on the finite declared concepts and vocabulary,
never the clinical rows at a site. Discovery cannot estimate support.

Execution checks the capabilities required by the actual bound plan at every
site. Defaults remain memory output and `on_small_site = "error"`. Staging and
small-site skipping are explicit execution options. Only publication roots
are support-gated; no library per-feature/per-arm gates, budgets or state are
introduced. Intermediate tables and patient rows stay on the server.

The immutable identity is `(id, template_version)`. A NULL version resolves
only the installed recommendation. Definition checksums identify unbound
artifacts and contextual constraints; actual bound semantic digests identify
execution semantics. Neither identifies a database snapshot. Client provenance
survives JSON/YAML export/import. Edited recipes are customised derivatives,
not certified equal to the pinned entry. Parameter constraints also apply when
binding an exported/imported template later.

Use ordinary recipe editing and `recipe_merge(..., prefix = ...)` for
composition. Shared parameter definitions must agree. Prefixing preserves
references but changes plan identity and can change deterministic matching.
Merging separate rosters does not establish a join between their episode IDs.
The active-comparator template demonstrates private baseline/survival joins
within one roster, publishing only the joined outputs.

Scope limits: adherence provides numerator/denominator components, not automatic
ratios; scores retain the five existing mappings and have no lookback parameter;
new-user washout excludes event starts, not every overlapping supply interval;
matching is deterministic risk-set sampling, not propensity matching; SCCS
provides exposure partitions, not an estimator. Social questions, drug classes
and outcomes require analyst definitions. No new server endpoints, SQL, wire
fields, parameter types, configuration or persistent registry are needed.

Package versions stay paired at the integration release. The catalogue's
minimum target client release is 2.10.5; its server requirement is the integrated
2.10.4 native capabilities. Vendor and release acceptance are recorded in the
implementation report rather than inferred from this documentation.

## Output schemas

Every assigned patient component retains the native identity columns, in order:
`row_id`, `cohort_row_id`, `person_id`. Wide outputs then contain the variables
listed by their entry (schema `K` in the entry documentation). IDs use native
synthetic episode keys and existing person pseudonymisation. They are not
physical source event IDs.

Single-row survival (`S`) adds `outcome_name`, `event`,
`entry_days_from_index`, `exit_days_from_index`, `follow_up_days`.
Recurrent event components (`RE`) add `outcome_name`, `event`, `event_number`,
`outcome_event_number`, `event_days_from_index`, `entry_days_from_index`,
`exit_days_from_index`; risk sets (`RR`) add `entry_days_from_index`,
`exit_days_from_index`, `follow_up_days`. Elapsed follow-up is exit minus entry.

Dense periods (`PP`) add `period_start_days`, `period_end_days`, `days` and
the configured category/analysis columns. Measurement panels have no implicit
age/calendar columns; incidence adds `calendar_year`; SCCS adds `age`,
`age_band`, `calendar_year`. Calendar labels retain native ordered-factor
presentation. Observed `days` is an inclusive day count.

Event logs (`EL`) add `source`, `concept_id`, `start_days`, `end_days`, `value`,
`unit_concept_id`, `event_number`. The library never adds physical event IDs.

Each entry's `output_schema.json` records the complete native columns, types,
nullability and identity roles exercised by its synthetic default binding.
`describe()$output_schema` exposes that fixture contract. Measurement schemas
are conditional on the bound source/concept set and vocabulary; the recorded
single-concept schema is an example, not a promise to collapse several analytes.
Concept-valued fields may be presented as harmonised labelled factors while
retaining their OMOP concept identity; factor indices are never concept IDs.
