# dsOMOPClient 2.10.3 (in development)

Recipe language 2.0 (design-2.10.3). This section is the WS-0 skeleton; each
workstream fills its entries.

## Recipe language 2.0 (WS-8)
- New language-2 constructors (design section 2): `omop_concept_set()`,
  `omop_event()` with `omop_where()` / `omop_where_tree()` row predicates and
  `omop_unit()` normalisation, the bounded adapters `omop_episode_members()`
  and `omop_cost()`; criteria `omop_has()`, `omop_has_not()`,
  `omop_count_is()`, `omop_not()`, `omop_all()`, `omop_any()`,
  `omop_person()`, `omop_age_is()`, `omop_observed()`, `omop_in_cohort()`;
  `omop_window()`, `omop_bound()`, `omop_anchor()`; `omop_index()`,
  `omop_any_of()`, `omop_end()`, `omop_coverage()`; `omop_era()` and
  `omop_censor()` (with `when_index_source`, amendment A-7); `omop_reduce()`
  under the frozen reducer contract (including `covered_days(coverage =,
  observed_only =)` and `supply_days(fills =)`); `omop_age()` (with
  `bands` / `age_breaks`), `omop_sex()`, `omop_race()`, `omop_ethnicity()`,
  `omop_birth_year_band()`, `omop_died()`, `omop_days_to_death()`,
  `omop_n_observation_periods()`, `omop_obs_duration()`, `omop_prior_obs()`,
  `omop_followup()`, `omop_observed_days()`, `omop_score()`,
  `omop_population_column()`; `omop_match()`, `omop_age_band()`,
  `omop_strata()`; `recipe_concept_sets()`, `recipe_import_concept_sets()`,
  `recipe_export_concept_sets()` and `recipe_explain()` (its appended
  `symbol` names the server handle `omopPlanExplainDS()` reads).
- Every 2.10.1 export keeps its positional formals; new arguments are
  appended: `omop_variable(event, reducer, where, visit, broadcast)`,
  `omop_variable_block(where, visit, concept_col, visit_filter)` (F-24;
  `visit_filter` is the legacy alias of `visit`, giving both is an error),
  `omop_population(index, match, coverage)`, `omop_output(rows, grain,
  columns, format, sources, collapse)` (`type` now defaults to `NULL`; giving
  neither `type` nor `format` keeps `"wide"`), `omop_recipe(concept_sets,
  events, intervals, coverage, publish)`, `recipe_execute(on_small_site)`,
  `ds.omop.plan.execute(on_small_site)`, `recipe_to_plan(version = 2L)`.
- `recipe_to_plan()` emits a v2 plan by default (validated against the
  vendored `plan-v2.json`); `recipe_to_plan(version = 1)` returns the
  unchanged 2.10.1 plan of a recipe built from 2.10.1 constructors. A recipe
  built only from 2.10.1 constructors (and without `parent_id`) executes as
  that v1 plan on every server, so its results are unchanged; any other
  recipe executes as a v2 plan on servers advertising `recipe_language` 2
  and every operation it needs.
- With dsOMOP 2.10.3 servers the client reads each execution's publication
  status (`omopPlanStatusDS`) and takes symbol names from the published
  inventory (one symbol per survival TAR, fresh names on collision) instead
  of predicting them (A-E13); `on_small_site = "skip"` returns `NULL` for a
  site that refuses a release below its minimum number of persons and keeps
  the others (strict and non-strict servers, design section 7.3). Federated
  v2 executions bind the plan's semantic digest.
- `print()`, `recipe_explain()` and `recipe_lint()` state the defaults a
  recipe relies on: "follow-up ends 730 days after index" for a survival
  output without `tar` (F-4, the default is kept), the banded `age_group` of
  `baseline` versus exact age in `wide` (F-5), the unique/drop/drop coverage
  of indexed populations, `collapse`, `missing = "skip"`; class-2 columns are
  reported as INFO `quasi_identifier`; the `has_measurement` /
  `missing_measurement` pair is explained (B-7, unchanged v1 meanings; in v2
  `omop_has_not(event)` = no row, `omop_has_not(event, nonmissing =
  "value_as_number")` = no non-null value).
- Index-relative recency uses `days_since` / `days_until` / `first_date` /
  `last_date` reducers with an explicit `reference` anchor (F-18);
  `time_since` keeps its fixed-date meaning.

## Corrected
- (B-8) `parent_id` now restricts membership (see dsOMOP 2.10.3): a child
  population is a subset of its parent, as `print()` has always shown. A
  recipe with `parent_id` is compiled to a v2 plan and refused, naming
  `population.parent`, by servers that cannot apply it.
- (F-3) The client blocklist is identical to the server's class-1 patterns
  (it gains `_source_concept_id`, `stop_reason`, `lot_number`,
  `unique_device_id`, note/NLP fields, address/geo, provider, care-site and
  direct-identifier patterns); birth components are not blocked on
  assignment paths.
- Circe imports carry their language-2 form: the first primary event that
  satisfies the observation window (`select = "first_eligible"` over
  `omop_observed(before = PriorDays, after = PostDays)`, unique coverage),
  which is Circe's own order; expressions with inclusion rules keep the
  2.10.1 form.
- Recipe JSON export and plan JSON save keep every digit (`digits = NA`) and
  YAML 15 significant digits: numbers such as unit conversion factors no
  longer round to 4 / 7 digits on export.

## Newly permitted
- No client-side mirrors of removed server gates (F-23): `recipe_lint()` has
  no `narrow_filter` / `unbound_numeric_filter`, `.classifyFilterClient` is
  removed, `omop_filter_value()` without `safe_bins` is the plain comparison
  `>=` / `<=` and `omop_filter_has_measurement()` takes a one- or two-sided
  numeric range without issued bins; custom row predicates accept ordered
  comparisons. Defaults are surfaced by `print()`, `recipe_explain()` and
  `recipe_lint()` instead.
- (F-17) `omop_filter_date_range()` accepts one-sided ranges (`>=` / `<=`).
- (A-E5) `-Inf` is accepted as an unbounded window start and `Inf` as an
  unbounded end (also for a survival `tar$end_offset`); reversed infinities
  are errors.
- Exact birth components and exact age may be requested in assigned tables
  (class 2, released only through the final minimum-persons rule).

## Newly refused
- Plans that do not validate against the vendored `plan-v2.json` are never
  sent; a v2 recipe whose operations an older server lacks is refused with
  the operation named (parent inheritance, coverage policies, event
  censoring and matching are never dropped to fit an older server).
- `recipe_to_plan(version = 1)` refuses language-2 constructs, naming them.
- A language-2 survival output needs an explicit `censor` list; class-1
  columns are refused at compile time in projections, value columns and
  predicates of v2 plans.

## Contracts shipped by WS-0
- Vendored `inst/schema/` (byte-identical to dsOMOP's), `R/json_schema.R`,
  `R/semantic_digest.R`; `openssl` added to Imports for the semantic digest.
  The digest's number serialisation finds the ECMAScript shortest round-trip
  decimal for every finite double (power-of-two regression `2^-24` ->
  `5.960464477539063e-8`; digest vector `power_of_two_factor`).

# dsOMOPClient 2.10.2

- Version aligned with dsOMOP 2.10.2 (which reads every OMOP CDM version
  label); no client change.

# dsOMOPClient 2.10.1

- `ds.omop.connect()` reports each server's own error instead of DSI's
  generic "There are some DataSHIELD errors, list them with
  datashield.errors()", followed by what to do when that is known. A server
  whose DataSHIELD profile does not publish dsOMOP's methods (dsOMOP missing,
  or upgraded on Opal without publishing its settings again) now stops the
  connection before anything is assigned, naming the missing method and the
  Opal page where an administrator publishes it.
- New `ds.omop.client.version()`: the installed dsOMOPClient version.
- New `ds.omop.server.version(conns)`: works on plain DataSHIELD connections,
  before `ds.omop.connect()`. Per server it reports the dsOMOP version whose
  methods the profile publishes, the version that answers (the installed
  one), the methods this client calls that the profile lacks, whether the
  server is ready, and what to change when it is not.
- Pairs with dsOMOP 2.10.1, which never requires TLS on a database
  connection, reads dsOMOP 1.x resources and finds the CDM schema of a
  resource that names none.

# dsOMOPClient 2.10.0

- Support dsOMOP 2.10.0's seed-only DP: servers reporting
  `release_binding = "seed_data_versioned_v1"` (no stored answers; answers
  are deterministic from the server's secret root, the question and the
  data) are accepted for `ds.omop.dp.release()` and the sticky QueryLibrary
  releases, alone or mixed with first-answer servers (dsOMOP 2.7.1 to 2.9.x).
  `mixed_contracts` now also reports such a mix. The seed stays entirely
  server-side; the client only learns the new contract label.
- Help pages, README and vignettes describe both server generations; an
  incoherent v3 status is now reported as "incoherent v3 DP contract".

# dsOMOPClient 2.9.1

- New `ds.omop.dp.prepare()`: prepares a person-local table for sticky DP
  releases from an `omop_recipe` or `omop_plan` (server method
  `omopDpPlanExecuteDS`, dsOMOP 2.9.1). Loading tables with
  `ds.omop.plan.execute()` and `recipe_execute()` is unchanged and never uses
  DP. Prepared tables skip concept-factor harmonization, which would drop the
  DP seal.
- `ds.omop.querylibrary.sticky.release()` works in one call: without `x` it
  prepares the input from the redesign's `preparation` recipe or plan into a
  temporary server symbol, releases, and removes it. The redesign can be the
  first argument.
- `omop_querylibrary_sticky_catalog()` gains `automatic_equivalent`: the
  analysis-catalog entry that answers the question with no preparation
  (dsOMOP 2.9.0), also shown when a redesign is printed and suggested when a
  release has no input.
- Plan execution errors now include each server's own reason.

# dsOMOPClient 2.9.0

- Support dsOMOP 2.9.0's automatic protection of the OHDSI community tools.
  `ds.omop.analysis.run()` keeps each server's protection label in
  `meta$protection` (`"standard"`, `"dp"` with epsilon, sensitivity and the
  noise standard deviation, or `"standard_fallback"`), and the result prints
  it with the estimand. `ds.omop.query.exec()` keeps the labels in the
  `dsomop.protection` attribute.
- The reopened query-library questions (per-person durations and counts,
  demographic distributions, values, quantities and costs) pool through their
  server-owned contracts like every other analysis.
- When federated analysis metadata differ because servers use different
  `dsomop.tools.mode` settings, the error names each server's mode.
- `ds.omop.dp.status()` prints each server's tools mode.
- Documentation: sticky QueryLibrary redesigns take their input from a
  community-tool loader (an assign-mode analysis-catalog entry), because plan
  and recipe outputs carry no DP provenance since dsOMOP 2.8.0; the common
  per-person questions need no preparation in 2.9.0.

# dsOMOPClient 2.8.0

- Loading tables never depends on differential privacy (DP).
  `ds.omop.plan.execute()` no longer requests DP status before execution and
  always performs the requested concept-factor harmonization, as before DP
  was added. Servers 2.7.x with exclusive DP refuse factor discovery: set
  `factor_concepts = FALSE` there, or upgrade them to dsOMOP 2.8.0.
- The shared aggregate helper no longer turns DP-exclusive refusals into a
  special fatal error. It reports clearly when a server has the OHDSI
  community tools disabled (`dsomop.tools.enabled`), and analysis failures now
  include each server's reason.

# dsOMOPClient 2.7.4

- Support dsOMOP 2.7.1's v3 snapshot-first-answer contract for
  [isglobal-brge/dsOMOP#20](https://github.com/isglobal-brge/dsOMOP/issues/20).
  Validate the persistent binding and public storage-capacity status fields,
  and include them in federation compatibility and result metadata.
- Keep legacy v2 status inspectable and explicitly labelled as old. Release
  calls require the new contract by default. The opt-in client option
  `dsomop.dp.allow_legacy_servers = TRUE` supports staged upgrades, including
  mixed v2/v3 federations, with a warning naming every legacy server: no
  first-answer binding: an unrotated data refresh can reveal whether a released
  statistic changed; see isglobal-brge/dsOMOP#20. Results retain each server's
  contract and legacy warnings, including pooled-only views; shared contract
  fields that differ across sites are `NULL`. All mechanism, provenance,
  harmonization and per-site payload checks remain in force.
- Document permanent first answers, stale-until-custodian-rotation semantics,
  automatic server-store initialization with a local identity pin, recommended
  external pinning, persistent state recovery and the default 1 GiB public
  reservation capacity. All seven primitive calculations, sensitivities,
  epsilon/delta and pooling formulas remain unchanged.

# dsOMOPClient 2.7.3

- Adapt memory-mode plan and recipe execution to dsOMOP 2.7.0's default
  exclusive DP channel: automatically disable observed factor-level discovery
  across the selected federation with an explanatory message.
- Validate and print the DP status's `exclusive` flag, retaining compatibility
  with dsOMOP 2.6.0, which omits it.
- Standard statistics helpers now surface exclusive-mode refusals as clear
  errors directing analysts to `ds.omop.dp.release()`; no partial standard
  result is returned when a selected server refuses the channel.

# dsOMOPClient 2.7.2

- Document the server 2.6.0 default-enabled DP release channel, custodial opt-out,
  derived resource/snapshot identities, privacy epoch and persistent state
  requirements. Client release calls remain explicit; no API defaults change.
