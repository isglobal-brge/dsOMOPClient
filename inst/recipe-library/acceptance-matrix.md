# Recipe library acceptance matrix (review R2)

Every path below is under
`tests/testthat/fixtures/recipe-library/<entry>/1.0.0/`. `default/` is the
previously reviewed default bundle. Every named variant has `bindings.json`,
`recipe.R`, independently constructed `control.R`, `plan.json`, `normalized.json`,
`semantic.json`, `semantic.sha256`, `expected.json`, `schema.json`, and
`manifest.json`. Refusal bundles verify rollback in memory and staged modes and declare no
published components or schema;
they retain the complete valid plan and the expected refusal class.

`test-recipe-library-variants.R` runs every named variant through public
`recipe_execute()` with nfilter=5. Successful cases check memory and staged
values, component inventory, protected identities, full released schemas,
explicit/default/staged bindings, cross-dialect matching membership after test-only
identity resolution, literal-control equality, JSON/YAML/code
round trips, schema validation, normalisation idempotence, shared server/client
normalisation, semantic payload and digest. Changing a used binding must
change the default plan's digest. The fixture helper authors expected rows
from study constants and arithmetic, never from executor output.

| Requirement | Entry | Exact variant cases and independent expectation |
|---|---|---|
| T01 | demographics.baseline | `repeated_partial`: select all, Dec-31 births still age 40, two episodes/person with 5/6 prior days; `missing_demographics`: NA; `ambiguous_drop`: refusal after unique-coverage drop. |
| T02 | socioeconomic.education | `tied_zero`, `last_missing`, `empty_questions`: three tied, conflicting answers resolved by event key; zero stays zero, final missing stays missing, counts include all three; empty gives 0/NA. Default excludes employment questions. |
| T03 | socioeconomic.employment | `tied_zero`, `last_missing`, `empty_questions`: independent employment source/output and selected question domain. Default coexists with education questions without including them. |
| T04 | socioeconomic.numeric_observation | `affine_value`: 4*2+1=9; `affine_missing`, `affine_exclude`: last unknown unit yields NA/count 2 or prior 9/count 1; `last_row_missing`: final raw NA retained. `test-recipe-library-units.R` refuses undeclared/incomparable policies through all binding routes and checks explicit restriction/conversion. No currency conversion is inferred. |
| T05 | demographics.death_cause | `day_0`, `day_10`, `day_11`: inclusive [0,10], coded zero, outside-horizon absence; `first_missing`: first missing cause cannot be replaced by the later duplicate death record. Default tests no in-window death. |
| T06 | comorbidity.charlson | `before_index`, `indexed_duplicates`, `future_excluded`: diabetes 1/1/0; `hierarchy`: declared descendant contributes once; `missing_required_input`: refusal. Default zero-history control. |
| T07 | comorbidity.chads2 | `before_index`, `indexed_duplicates`, `future_excluded`: diabetes plus age-75 contribution 2/2/1; `age_threshold`: 74/75 gives 0/1 without conditions; `missing_age_sex`: 0; `missing_required_input`: refusal. |
| T08 | comorbidity.chadsvasc | `before_index`, `indexed_duplicates`, `future_excluded`: diabetes + age-80 + female = 4/4/3; `age_threshold`: male age 74/75 = 1/2; `missing_age_sex`: 0; `missing_required_input`: refusal. |
| T09 | comorbidity.dcsi | `before_index`, `indexed_duplicates`, `future_excluded`: mapped retinopathy 1 + nephropathy 2 = 3/3/0; `highest_severity`: same-category tiers max(1,2)=2; `vocabulary_mismatch` and `missing_required_input`: refusals. |
| T10 | comorbidity.hfrs | `before_index`, `indexed_duplicates`, `future_excluded`: mapped F05 delirium 3.2/3.2/0, duplicate never doubles; `missing_required_input`: refusal. |
| T11 | comorbidity.condition_history | `window_duplicates`: [-10,-1] includes two distinct days despite duplicates; `empty`: 0/0; `unavailable`: NA/NA; `rare_private_feature`: one positive person retained inside 24-person final roster. |
| T12 | drug.era_summary | `gap_nested_p0`, `gap_nested_p1`: nested fills, gaps exactly 2 versus 3, persistence changes course envelopes but recorded coverage stays 25; `supply_fallback`: recorded ends, missing ends/supply, invalid supply with/without ends, and reversed ends. |
| T13 | drug.adherence_components | Default recorded union 44/effective 60. `union_4_of_4`: observed [0,1] union [8,9] gives covered/observed 4/4; `carry_in`: pre-window supply contributes 5; `cap_binding`: A-20 200/10/10-day fills, previous-effective-end cap gives union 200, supply 220; `kept_uncovered`: observation-dependent values NA (raw clipped supply remains 10); `unavailable`: source-dependent values NA. |
| T14 | cohort.new_users | `later_eligible`: first candidate under-observed, later qualifies; `zero_washout`: two separate courses, no washout filters, all episodes; `prior_overlap`: start outside washout despite overlapping supply, separate ingredient groups; `same_day_ingredients`: two mapped groups produce two episodes. Default uses unmapped ingredient's own group. Default test also pins bound/default/literal zero expansion. |
| T15 | cohort.active_comparator | `dual_same_day`: A wins; `own_restart`: ITT/on-treatment 100, restart does not switch; `opposite_switch`: on-treatment 50, ITT 100; `short_entry`: day-0-only course excluded from day-1 on-treatment while ITT retains it; `prior_on_index`: excluded; `event_censor_tie`: event wins at 50; `prior_start_990006`, `prior_start_990007`: prior start in either arm excludes affected persons. Exactly two joined roots. |
| T16 | outcome.incident_event | `prior_10`, `prior_11`: inclusive lookback exclusion versus outside-window acceptance; `later_candidate`: first lacks full observation and later qualifies; `empty`: refusal. Same-day duplicate outcomes never multiply the selected first-eligible episode. |
| T17 | survival.first_event | `boundary_0`, `boundary_1`, `boundary_10`, `boundary_11`: index/entry/horizon edges, prior_days=0, elapsed zero at entry=exit; `death_tie`: event wins; `gap_censor`: last observed day 3 censors, event in gap ignored. Default includes event/no-event and no index-end cap. |
| T18 | survival.competing_risk | Default different-day causes. `tie`, `overlap`: A priority; `death_cause`: explicit death cause B at day 3; `censored`: NULL cause. First-event `death_tie` separately exercises death only as censor with the same event-wins policy. |
| T19 | survival.recurrent_events | `washout_boundary`: raw-event washout suppresses days 3/4, retains 1/7, same-day priority; `empty_sentinel`: event 0, event_number 1, NULL cause; `zero_risk`: entry=exit, elapsed 0; `rare_complete`: four event persons plus 20 sentinels keep both components supported; `component_support_refusal`: explicit complete=FALSE edit leaves only four event persons despite 24 risk-set persons, so whole publication refuses. |
| T20 | panel.measurements | Default one concept. `two_concepts`: both declared concepts have records; `empty`: same two-concept dense schema with 0 counts/NA values; `unavailable`: all source values NA; `carry_equal`, `carry_expired`, `lookback_outside`: equality and one-day-outside boundaries. Period-start values qualify and future values never carry backward. Both wire and vocabulary-labelled schema names are pinned. |
| T21 | panel.incidence | `year_gap`: leap-year, partial years, actual gap excluded and periods split at coverage boundaries, repeated events counted; `repeated_empty`: overlapping same-person episodes retained separately, zero-event bins. |
| T22 | panel.sccs | `interior_11_days`: observed [0,99], requested [19,29], no interior exposure boundary, overlapping exposure union gives 11 days; `empty_year_gap`: empty exposures, actual gaps exclude outcomes, Jan-1 annual age 40 to 41. |
| T23 | longitudinal.event_log | `repeated_namespaces`: identical physical keys from two namespaces, same-date total order, numeric/NULL values and repeated episode association; `empty_source`: available-empty source doesn't erase other rows; `empty_final`: final support refusal. |
| T24 | matching.nested_case_control | `same_time`, `death`, `gap`, `missing_strata`: disqualified controls leave supported zero-control cases; `earlier_ineligible`: first ineligible event remains exclusion history; `future_cases`: future cases can control earlier cases, later cases have no eligible controls; `ratio_seed`: nondefault ratio/seed and weights; `candidate_cap`: technical refusal with rollback; `distinct_person_support`: nine matched rows from four genuine persons still refuse at nfilter=5. Existing boundary test below supplies membership/reordering/seed sensitivity. |

Additional exact tests, not generic claims of language coverage:

- `test-recipe-library.R`, “all definitions retain deferred prefix semantics and
  origin provenance”: all 24 exact library templates, binding before/after the
  same `lib` prefix, including the comparator censor source tags and panel carry
  keys. The per-entry frozen-plan test checks the literal §4 control, defaults,
  staged bindings and parameter/digest sensitivity.
- `test-recipe-library-boundaries.R`, “risk-set sampling retains seed sensitivity,
  row-order reproducibility and cap refusal”: exact T24 default expansion on two
  sites with reverse physical row order, independent person-token-to-fixture
  membership comparison, seed 1 versus 2, and max_candidates=1. Prefixing is not
  asserted to preserve sample membership. Reused controls belong to distinct
  case sets under the native episode contract.
- The same file's support, federation and missing-table tests use exact library
  recipes and preserve default all-site failure, explicit skip and rollback.
- `test-recipe-library-empty-comparator.R`: exact T15 defaults on two sites,
  with site2's drug source empty or all candidates excluded by the existing
  observed-history criterion. In memory and staged modes, default execution
  reports `insufficient_support` and restores both sites' symbol inventories
  and staged files. Explicit skip retains both supported-site roots, with all
  rows checked against the independent default oracle and across output modes.
- `test-recipe-library-units.R`, “mixed-unit library execution refuses unqualified
  values and honours explicit policy”: exact review reproduction (6 in unit
  8840, 8000 in 8554), refusal before assignment; conversion gives mean 7/last 8,
  restriction gives mean/last 6.

`test-recipe-library-ratios.R` executes the separate protected mutation example
already present in the T13 README. Starting from the exact `union_4_of_4`
library case, two sites produce PDC 1 and MPR 2.5; zero/NA denominators yield
NA, keys stay unchanged, and a refused in-place key edit restores the previous
result at both sites. This is an existing-verb test, not another template.

Full suites remain integration/release work. Run manifests and the fix report
record the exact targeted runs and revisions.
