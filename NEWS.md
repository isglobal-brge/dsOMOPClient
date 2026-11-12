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
  that v1 plan, so its results are unchanged, unless the v1
  compiler refuses one of the shapes 2.10.3 newly permits (below); such a
  recipe, and any recipe using language-2 constructs, executes as a v2 plan
  on servers advertising `recipe_language` 2 and every operation it needs.
- The legacy `survival`, `intervals`, `temporal_covariates` and
  `person_period` presets lower to their language-2 formats with the legacy
  defaults (TAR 0 to 730 days, censoring at the observation-period end, at
  death when `censoring$death = TRUE` and at the cohort end); a legacy
  multi-state survival output keeps its v1 plan.
- `omop_where()` admits an inline `omop_param()` placeholder (the only call
  it evaluates, with literal arguments; A-13).
- Plan selection (A-14) is one rule for `recipe_execute()`,
  `recipe_validate()`, `recipe_preview()`, `recipe_explain()` and
  `recipe_lint()`. `recipe_explain()` of a recipe that executes as its v1
  plan explains that v1 payload and its legacy rules (with `conns`, the
  server's lowered IR and digest) and never presents the native rendering as
  the executed plan. With `conns`, only the sites whose profile publishes
  `omopPlanExplainDS` are asked; the others and every failed call are
  reported by site, a v1 explanation marks its lowered IR and digest
  unavailable when no site explained it or the sites disagree, and the
  execution route (protocol, legacy call or refusal) is stated (review B7).
  A recipe with neither a v1 plan nor a native rendering is refused naming
  both causes; native-rendering limitations direct to
  `recipe_to_plan(version = 1)`.
- Language-2 plans always carry an explicit `publish` list: the recipe's
  roots, else every declared output; an execution with a partial `out`
  publishes exactly the mapped roots.
- Temporal-covariates and person-period outputs whose variables request
  different analyses per concept emit per-source analyses
  (`source_analyses`, A-14): only the declared (concept, analysis) pairs are
  computed. A `covariates_sparse` output, which has no per-source selection,
  refuses them; `options$analyses` naming an analysis no variable requests is
  refused.
- Names follow A-9: an imported Circe concept set is
  `.sanitize_name(<name>)` (fallback `concept_set_<CodesetId>`); an unnamed
  event is `<concept set>_<table>` with exactly five abbreviations (`dx`,
  `rx`, `px`, `lab`, `obs`) and the full table name otherwise; collisions take
  the first free `_2`, `_3`, ... without truncating the suffix.
- The normal form and semantic digest of language-2 plans come from the
  vendored A-11 normaliser (`R/plan_normalize_v2.R`), whose grain rule
  follows the left member of an intersect or setdiff.
- Language-2 plans execute through the same publication protocol as v1
  plans (status, inventory, `on_small_site`, rollback; see Corrected and
  Newly permitted), sent losslessly after every server's recipe-language
  capabilities are checked; federated v2 executions bind the plan's semantic
  digest.
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
- Named events and intervals are declarative (review N1): a declaration may
  reference one declared after it; a dependency cycle is refused, naming it
  (`a -> b -> a`).
- Population rules are the server's (`S/R/ir_validate.R`; review B3). The
  cases of a match may be any indexed population: an index population, a
  criteria child of one, or a set operation that A-11 counts as indexed (a
  union when any member is, intersect and setdiff after their left member).
  A population used only as a matching controls template may carry an
  index-referenced `omop_age_is()`, evaluated at case time; one that is also
  an output's rows, a parent, a set-op member or match cases still needs an
  index. Episode grain over a population without an index is refused by
  `recipe_to_plan()`.
- `omop_observed(gap = )` with any non-NULL gap, 0 included, needs the
  server operation `population.coverage.union`; the client checks it before
  any assignment (review B2).
- `publish = character(0)` (also `recipe_set_publish(recipe, character(0))`)
  publishes nothing: the plan carries `publish = []` through JSON, YAML,
  `recipe_to_code()` and plan files, the servers validate it and report an
  empty inventory, and execution returns an empty mapping; `NULL` still means
  every output (review B6).
- `recipe_export_concept_sets()` refuses a nested exclusion (an exclusion
  with its own exclusion), which an OHDSI concept-set expression cannot
  carry, instead of flattening it (review B4).
- `ds.omop.server.version()` tells an old installation (dsOMOP before 2.10.4,
  where publishing again cannot add the protocol methods: install 2.10.4 and
  publish its methods) from an upgraded server whose profile still publishes
  the old methods (publish again), and adds the list column
  `recipe_language`: each site's capability block (`version`, `operations`,
  `dialect`), `"unavailable"` for a site that predates it and `"not
  connected"` without an OMOP session (review N2).

## Corrected
- Plan execution reads authenticated publication status before cleanup and uses
  the server inventory, including freshly allocated component symbols. Technical
  and transport failures revoke staged files as well as assigned outputs.
- A partial `out` mapping without an explicit `publish` requests only the
  mapped outputs. For example, `out = c(conditions = "cond_df")` publishes
  only `cond_df`; the server leaves other computed outputs private and the
  client accepts the matching publication status.
- (B-8) `parent_id` now restricts membership (see dsOMOP 2.10.3): a child
  population is a subset of its parent, as `print()` has always shown. A
  recipe with `parent_id` is compiled to a v2 plan and refused, naming
  `population.parent`, by servers that cannot apply it.
- (F-3) The client blocklist is identical to the server's class-1 patterns;
  birth components are not blocked on assignment paths. Informational
  `quasi_identifier` lint identifies birth, age and date variables without
  changing compilation or aggregate admission.
- Circe imports carry their language-2 form: the first primary event that
  satisfies the observation window (`select = "first_eligible"` over
  `omop_observed(before = PriorDays, after = PostDays)`, unique coverage),
  which is Circe's own order; expressions with inclusion rules keep the
  2.10.1 form.
- Recipe JSON export and plan JSON save keep every digit (`digits = NA`) and
  YAML 15 significant digits: numbers such as unit conversion factors no
  longer round to 4 / 7 digits on export.

## Newly permitted
- (B-20) `omop_index_event()` accepts every event table of the server's role
  registry (vendored `inst/ohdsi/roles-5.3.csv`, `roles-5.4.csv`): besides the
  seven tables of 2.10.2, `visit_detail`, `observation_period`, `death`,
  `specimen`, `note`, `payer_plan_period`, the era tables and `episode`; the
  server checks the table against its own CDM version.
- `on_small_site = "skip"` on plan and recipe execution keeps successful sites
  while asking each server to commit all output families or none. Non-strict
  partial results warn with rejected family names and codes.
- Numeric row thresholds and one-sided measurement criteria compile without
  safe bins; supplied bin contracts retain their scope checks. Removed the
  `narrow_filter` / `unbound_numeric_filter` lints and filter safety categories.
- Circe round-trips one-sided measurement bounds with identical endpoint
  semantics; half-open two-sided ranges and authenticated bin contracts still
  refuse export because Circe cannot preserve them.
- (F-17) `omop_filter_date_range()` accepts one-sided ranges (`>=` / `<=`).
- (A-E5) `-Inf` is accepted as an unbounded window start and `Inf` as an
  unbounded end (also for a survival `tar$end_offset`); reversed infinities
  are errors.
- Exact birth components and exact age may be requested in assigned tables
  (class 2, released only through the final minimum-persons rule).
- (design 6.6) `omop_filter_group(operator = "NOT")` negates the conjunction
  of its children: the legacy spelling of `omop_not()`.
- (B-22) `omop_output(type = "covariates_sparse")` is the language-2
  `format = "sparse"` over the output's variables; `format = "membership"`
  and `"dictionary"` are reachable too.
- (B-19) A feature output may mix variables with and without an
  index-relative window: one roster with one reducer window per variable;
  a windowed features output without an explicit grain takes episode grain
  (A-14) and needs an indexed population; with an explicit grain (such as
  `options = list(grain = "person")`) it keeps the 2.10.2 refusal (review
  B5).
- (design 2.2, 6.12) Survival outcomes and interval or temporal-covariate
  sources may be narrowed by visit or concept column; an intervals output
  may mix concept-scoped and unscoped variables of one table (one source per
  scope); a temporal-covariates output may declare different analyses per
  concept (per-source analyses, A-14).
- These shapes have no v1 rendering: they run on dsOMOP 2.10.4 servers;
  `recipe_lint()` reports them as INFO `language_2_only`.

- (F-1) New `omop_filter_concept_set(concept_ids, concept_col = NULL)`: a
  row concept-set filter matching the output table's domain concept column
  from the role registry (`condition_concept_id` for `condition_occurrence`,
  ...), or `concept_col`. Row concept-set filters compiled to the column
  `concept_id`, which the server refused.

- (A-18) The section 11 adapter refusals reach the analyst as the error
  `Plan publication refused: <site>:<output> [validation_error] (<reason>)`
  with `adapter_person_mismatch` or `cost_currency_mixed`, under both
  `on_small_site` settings (they are never skippable), and nothing is
  published; tested end to end through DSLite against a PostgreSQL CDM.

## Newly refused
- (F-2) A recipe-level row filter is attached only to outputs whose event
  table has every column it references; otherwise `recipe_to_plan()` stops
  naming the output and the missing column (the filter was attached to every
  output and failed at the server). Table-agnostic date and concept-set
  filters still apply to every output, and columns no CDM table defines are
  left to the server. Put the predicate on an `omop_event(where = ...)` of
  the right table instead.
- Invalid or incomplete publication status fails closed; only
  `insufficient_support` can skip a site.
- Plans that do not validate against the vendored `plan-v2.json` are never
  sent; a v2 recipe whose operations an older server lacks is refused with
  the operation named (parent inheritance, coverage policies, event
  censoring and matching are never dropped to fit an older server).
- `recipe_to_plan(version = 1)` refuses language-2 constructs, naming them.
- A language-2 survival output needs an explicit `censor` list; class-1
  columns are refused at compile time in projections, value columns and
  predicates of v2 plans.

## Shared with dsOMOP 2.10.3 (WS-2)
- The language 2.0 normal form is the frozen, byte-identically vendored
  `R/plan_normalize_v2.R` (`.planNormalizeV2()`, amendment A-11); the
  server's `.irNormalize()` calls the same function, so normalised forms and
  semantic digests agree on both sides. WS-2's interim `R/ir_defaults.R`
  (`.irNormalizeWire()`) is removed.
- The role-registry tables (`inst/ohdsi/roles-5.3.csv`, `roles-5.4.csv`) are
  vendored byte-identically from dsOMOP.

## Upgrading clients and servers
- Before anything is assigned, every execution checks the methods each
  site's profile publishes (review B1). With every site on dsOMOP 2.10.4 the
  publication protocol runs (`omopPlanStatusDS`, `omopStagedRevokeDS`,
  `commit_policy`). While any site lacks it, a language-2 plan and
  `on_small_site = "skip"` are refused before any assignment, naming the
  sites, the missing methods and the upgrade (install dsOMOP 2.10.4 and
  publish its methods). A v1-selected recipe or plan then runs on every site
  as the legacy call of dsOMOPClient 2.10.2 (the same v1 payload, no
  `commit_policy`, no status or revocation calls) with one warning naming the
  old sites: cross-site rollback and the section 7.3 publication guarantees
  do not apply until they upgrade, and each server applies its own
  disclosure rules. DP preparation and the sticky query library follow the
  same rule. A site whose profile cannot be listed, or that lacks a method
  the call needs on either route, stops the execution before anything is
  sent. Check with `ds.omop.server.version()`; on Opal, publish dsOMOP's
  methods again after upgrading.
- With a 2.10.1 or 2.10.2 client and a 2.10.4 server, a strict publication refusal
  still publishes nothing. The assign call returns status `refused`, and the
  old client then reports that requested outputs did not materialize instead
  of receiving the previous constant `Disclosive` error.

## Contracts shipped by WS-0
- Vendored `inst/schema/` (byte-identical to dsOMOP's), `R/json_schema.R`,
  `R/semantic_digest.R`; `openssl` added to Imports for the semantic digest.
  The digest's number serialisation finds the ECMAScript shortest round-trip
  decimal for every finite double (power-of-two regression `2^-24` ->
  `5.960464477539063e-8`; digest vector `power_of_two_factor`).
- Vendored `R/plan_normalize_v2.R` (amendment A-11): `.planNormalizeV2()` is
  the one normal form the semantic digest hashes on client and server; the
  normalised fixtures of `inst/schema/normalized/` are pinned. `plan-v2.json`
  gains `source_analyses` on temporal and person-period outputs (A-14).
  The vendored probe corpus carries the A-15/A-16 IR-only variants, refused
  on the wire (P64-P66).
- A-18 `.plan_validate_status()` admits the optional closed `reason` of a
  rejected entry (only with `validation_error`); the refusal diagnostic names
  it with the output and code (`site:output [validation_error] (reason)`).

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
