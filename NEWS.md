# dsOMOPClient 2.10.4 (2026-11-13)

- Corrected: assigned age bands, birth-year bands and panel/calendar categories
  use the controlled factor reconciliation even when concept-label translation
  is disabled. Re-run recipes against the corrected server to recreate factors
  with shared levels; explicit numeric date representations remain numeric.

## Date presentation (A-28 / CR-29)

- Corrected (DATES-B4): year-bearing custom factors include every year in a
  declared `level_range`, including unused middle years and ISO boundary weeks.
  Empty inputs stay empty, including patterns with literal separators. Empty
  or disjoint observed dates no longer cause valid public keys to refuse
  publication. Re-execute affected recipes; no declaration changes are needed.

- Corrected (DATES-B1–B3): native date reducers and raw date columns apply the
  effective invalid/domain/future policy after selection and before presentation.
  A selected date beyond `as_of` becomes NA; no earlier event is substituted.
  Yearless custom factors use first appearance in exactly 2000–2399, and escaped
  percent directives are literal text in validation, rendering and label keys.
  Re-execute affected recipes to refresh values and ordered-factor contrasts;
  no declaration changes are needed. Active `%Y-W%V` remains refused: use `%G-W%V`.

- Newly permitted: `omop_date_spec()`, `omop_date()`, `omop_date_components()`,
  `omop_date_parts()`, `omop_time_between()` and `omop_age_at()` compose native
  calendar dates, periods, partial dates, date parts and independently anchored
  ages. Variable fields override recipe defaults. Calendar, fiscal, ISO/CDC and
  explicitly profiled epi weeks support ordered factors, numeric coordinates
  and inclusive period bounds. English names and validated custom patterns are
  locale independent. Date factors reconcile through the existing controlled
  factor transport, including public zero-count categories.
- Corrected: exact-date assignment needs no `dsomop.allow_absolute_dates`
  setting. All date-derived forms retain class-2 provenance, and publication,
  protected lineage and aggregate admission remain enforced. Native presentation
  cannot be erased by an omitted legacy date policy. Numeric measures stay
  numeric. Legacy `first_date`/`last_date` offsets and `omop_age()` annual ages
  keep their existing meaning when the new APIs are absent.
- Newly refused: unsafe/custom patterns (use `%G-W%V` for ISO weeks), incomplete
  epi profiles, irrelevant spec fields, duplicate part names and overlapping
  explicit legacy/native date policies. Express native policies in `date_spec`.
  Formats without a roster date slot refuse a date variable rather than
  encoding a Date/factor into a numeric sparse matrix.
- Migration: replace fixed-anchor offsets plus `mutate()`/`cut()` with
  `omop_date("first_visit", omop_date_spec(precision="quarter"))`; replace
  annual ages with `omop_age_at("death")` for completed years. Incomplete birth
  dates give NA unless an explicit imputation or `unit="annual_years"` is chosen.
  Coarsened labels are ordered factors; request `representation="number"` for
  trends or `"start"`/`"end"` for Date bounds. Native interval/panel/survival
  endpoint columns are added alongside unchanged internal day offsets when a
  recipe date spec requests presentation.

## Concept labels and categorical outputs

- Corrected: concept-valued outputs are unordered factors by semantic type,
  including renamed reducers, dictionaries and staged components. Vocabulary
  labels and generated names reconcile by concept ID across retained sites;
  absent categories retain their declared levels. Presentation-only requests
  may use different vocabulary versions. Computation continues to use IDs.
- Newly permitted: `labels` and `column_names` maps on `omop_variable()` and
  `omop_recipe()`, with variable maps taking precedence. Equal concept labels
  deliberately merge categories; column names must be unique valid R names.
  Use `options = list(translate_concepts = FALSE)` for numeric concept IDs and
  untranslated generated names. Ordinary age/calendar categories still reconcile.
- Newly permitted: `ds.omop.table(data, column, conns)` pools concept counts
  while preserving the complete permitted category contract, including unused
  zero-count levels and labels containing commas. Failed or suppressed site
  counts are never filled with zeros. Actual missing values stay missing.
- Corrected: concept model frames retain shared levels during subsetting.
  CSV staging distinguishes literal `"NA"` and leading backslashes from missing
  values through the descriptor's `backslash-v1` encoding. Consumers must use
  the descriptor-aware reader. Existing descriptors retain legacy decoding.
- Newly refused: malformed/empty label maps, colliding explicit column names,
  translation with `factor_concepts = FALSE`, and servers lacking the concept
  presentation capability. Remove that flag or disable translation explicitly;
  use matching 2.10.4 packages for the new presentation contract. Reconciliation
  failures roll back all sites and cannot be bypassed with small-site skipping.

## Native execution and compatibility

- Corrected: native outputs and reducers complete the integrated language-2
  path, preserving BIGINT values for empty/all-NULL streams, population
  membership, interval coverage, survival censoring and panel source identity.
  Concept reconciliation retains authenticated person lineage and snapshot
  provenance when resealing. Final publication and aggregate gates still apply.
- Corrected: v1 lowering retains supported legacy empty populations, owned
  cohort references, output shapes and score semantics in the compatibility
  layer. Native v2 keeps explicit coverage, anchors, censoring and grain rules.
  Unsupported or ambiguous forms fail with a typed reason; use explicit native
  declarations when migrating. See the population and extraction vignettes.

## Upgrading from 2.10.3

Install dsOMOP and dsOMOPClient 2.10.4 together, publish dsOMOP's methods and
Options on Opal, and check `ds.omop.server.version()`. Existing valid secrets
are reused automatically; no privacy setting, state provisioning, epoch bump,
ETL hook or TLS change is needed. Re-execute recipes to recreate categorical
columns. Use `as.character()` for factor labels, never numeric factor codes for
arithmetic. Legacy date offsets and annual `omop_age()` retain their meaning;
choose the native date/age helpers explicitly for new semantics. The protected
release protocol introduced in 2.10.3 is unchanged; pooling still requires
compatible contracts and logical source identities.

Release verification: [recipe/publication contract run 43515122503](https://github.com/davidsarrat/dsomop-opal-e2e/actions/runs/43515122503)
passed fresh and upgrade paths on Opal 5.7.6 and 6.1.3;
[full e2e run 43515127555](https://github.com/davidsarrat/dsomop-opal-e2e/actions/runs/43515127555)
passed both fresh and upgrade jobs. Both tested harness commit
`17e5df40f4718bdece091f68d3003c764f061854`. These are evidence for that
candidate, not a claim of live execution on every supported SQL dialect.

# dsOMOPClient 2.10.3 (2026-11-13)

Pairs with dsOMOP 2.10.3, the security release of the OHDSI-tools audit.

- Corrected named single-output transport: output mappings and other named
  scalar arguments now travel as one encoded JSON object, preserving their
  names without a `c()`/`structure()` expression. Single-output recipes and
  language-2 verbs run under the normal method allowlist; adapter refusals
  reach the analyst with their actual reason. Existing call syntax is unchanged.

- Release 2.10.3 pairs both packages, including recipe language 2 and the
  publication protocol. Existing valid secrets are reused; ordinary operation
  needs no privacy settings, state provisioning, epoch bump, ETL hook or TLS
  change. Opal's installed package and published profile remain distinct;
  the version check identifies a stale profile. README upgrade notes describe
  compatible pooling, legacy execution and changed-answer behavior.

- Corrected upgrade advice for the publication protocol backported to 2.10.3:
  an installed 2.10.3 with a stale 2.10.2 profile asks to publish its settings,
  rather than incorrectly requiring a further package upgrade.

- Protected histogram pooling retains its cells when a distribution gate
  withholds moments; unavailable means and standard deviations stay NA.
- New protected-release contract: dsOMOP 2.10.3 servers advertise
  `privacy_contract = "protected_release_fixed_basis_or_realised_v1"`.
  `ds.omop.dp.release()` validates their `dsomop-protected-release-v1`
  payloads (route, guarantee scope, signed components, no private material,
  no history-dependent fields) and pools them by summing the signed components
  of compatible sites and deriving each view once. Pooling a federation
  mixing 2.10.3 servers with per-release servers (2.7.1-2.10.2) is refused;
  `type = "split"` retains labelled per-site results. Every pre-2.10.3 server
  gets a plain upgrade warning: installing this client does not repair its
  server-side inference defects. The new server explicitly refuses typed
  status/release preflight from clients 2.10.2 and earlier.
- Heuristic payloads are never presented as differential privacy:
  `realised_sticky` releases report `mechanism_epsilon = NA`, a warning, and no
  pooled epsilon; `meta$privacy$protection` names each site's route. For
  fixed-synopsis releases the pooled epsilon is reported for disjoint sites
  (maximum) and overlapping sites (sum), applying to the declared fixed
  vectors before exact support gates. It is not an API-transcript guarantee.
- Standalone typed counts on 2.10.3 servers use the shared person-total
  allocation (0.2 of the reference calibration) and are noisier than before;
  typed bounded means need a registered public scale; typed histogram breaks
  are reported as requested and as effective (public-grid) breaks; a
  distinct-label cardinality is not pooled across sites.
- `ds.omop.analysis.run()` pools protected catalog releases (the
  `release_pooling` descriptor of 2.10.3 entries) from their signed components
  (one row per owner, stratum and interaction view: persons, mean, SD, events, person-time and
  rate where defined; pooled cells in the `cells` attribute). It never mixes
  standard and protected frames. Older clients see these entries as not
  poolable and display the per-server frames.
- Printed protection labels describe fixed-synopsis, heuristic
  realised-contribution and mixed protection and list the fields a server
  withheld and why.
- One logical node is never pooled twice: protected typed and catalogue
  pooling require every site's public logical source identifier
  (`source$logical_source_id`) and refuse a federation in which two
  connections name the same node, or in which a site's independence cannot be
  established (missing identifier). Per-site results stay available with
  `pool = FALSE` / `type = "split"`. Equal answers from independent sites
  still pool; source identity is never inferred from values.
- Guarantee claims are checked: a heuristic realised-contribution or mixed
  release (typed payload or catalogue protection label) carrying a finite
  mechanism or API-transcript epsilon, or a delta, is refused, as is a fixed
  synopsis without a finite mechanism epsilon and delta 0. Typed payloads
  and catalogue labels also require a guarantee scope coherent with their
  protection route, including the protected block of mixed frames. Only
  fixed synopses are printed with a delta.
- Typed pooling sums the signed components of the complete registered basis
  and derives each requested view once. Binary rates carry explicit numerator
  and denominator masks, including the nonmissing denominator; equal-width
  domains with different labels cannot pool. Weighted numeric records use
  one versioned complete-atom projection onto their protected weighted total,
  so one-site pooling and every displayed rebinning agree with the server.
- Person-time pooling reconstructs the complete gender, age and year
  hierarchy before selecting views, preserving parent event, time and capped
  recurrent totals and both margins of crossed views. Nested Achilles
  histograms retain their root's histogram margins. Versioned transportation
  display keeps converged Dykstra results and uses a deterministic feasible
  margin repair after its iteration limit; that fallback is not an exact L2
  projection. First-event and capped
  recurrent event-time histograms pool their complete signed day hierarchy,
  project it onto the person or recurrent event total, then select the
  requested day, month or year bins. Recurrent `persons` counts distinct
  contributors among the same K-capped occurrences using a complete canonical
  interval-presence vector (sensitivity 3K; fractions N/R .2 each, A/B/C .1
  each and P .3); coarse persons are never sums of daily persons.
  Its versioned feasible radial projection enforces event/person bounds and
  interval constraints, but retreat toward the event-mass anchor can add bias.
- Record-weighted protected releases (the eight QueryLibrary record
  summaries) are pooled by summing the signed persons, records and lattice
  sums per stratum and deriving the record-weighted mean and SD once (never
  weighted by persons); sites must share the entry, scale, bounds and cap.
  Versioned record displays constrain records to [N, K N]. Unscaled value
  histograms pool their signed cells onto that record total; pooled frames
  carry `value_histograms` and `value_quantiles` attributes with interval and
  tail-censoring information. A missing per-site histogram remains unavailable.
- Protected effect estimates (CohortMethod Poisson and grouped-time Cox,
  interaction estimates, the SCCS incidence rate ratio) are pooled, as in
  2.10.2, by inverse-variance meta-analysis of the per-site protected
  estimates; the signed-component pooling stays in the `protected_components`
  attribute. Cox exit-time cells are projected onto the pooled C6 event and
  non-event parents. Interaction C6/C8 pooling projects the complete signed
  atomic family before deriving requested subgroup views, retaining both
  subgroup and arm margins and explicit view identifiers. C8 uses finite IPF
  plus a feasible margin repair, not an exact KL/L2 projection. Missing or
  incompatible status/family metadata is refused. Cox cells are never
  refitted, and SCCS cells are never pooled.
- Servers without the DP service pool their standard outputs by the
  established contract; protected and standard frames are never mixed. Pooled
  standard-mode frames with protected person-time say that those fields are
  sums of protected values. The moment projection matches the server's.
- DP status: `snapshot_id` is a display label (servers 2.10.3 key fixed
  synopses on an automatic data generation they never release). Empty,
  absent or arbitrary display labels never affect protected-client admission.

## Recipe language integration

Recipe language 2.0 ships with typed constructors, client validation, v1
compatibility lowering and the server publication protocol. The entries below
describe the supported operations and migration paths.

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
- `ds.omop.server.version()` tells an old installation (dsOMOP before 2.10.3,
  where publishing again cannot add the protocol methods) from an upgraded
  server whose profile still publishes the old methods (stale publication), and adds the list column
  `recipe_language`: each site's capability block (`version`, `operations`,
  `dialect`), `"unavailable"` for a site that predates it and `"not
  connected"` without an OMOP session (review N2).

## Simple layer, parameters and verbs (WS-9)
- Simple layer (design section 3): `omop_study()` returns an ordinary
  `omop_recipe` -- one indexed population and the requested `"baseline"`
  (wide), `"survival"` (`competing_risk` with two or more outcomes),
  `"long"` (the outcome events) and `"panel"` (dense 30-day person periods)
  outputs, all with episode grain, priority ties and relative dates;
  `require` entries (`prior_observation`, `followup`, `age`, `sex`, `has`,
  `no_history_of`, `in_cohort`, criterion objects) expand one criterion each.
  `print()` lists every default the study chose, `recipe_explain()` states
  them (INFO `simple_layer` in `recipe_lint()`), `recipe_to_code()` prints the
  advanced expansion. `omop_new_users()` is the era-indexed population of
  first-eligible users after an observed washout. The shortcuts
  `omop_ever()`, `omop_count()`, `omop_first_value()`, `omop_last_value()`,
  `omop_mean_value()`, `omop_min_value()`, `omop_max_value()`,
  `omop_value_at_index()`, `omop_days_since()`, `omop_days_until()`,
  `omop_first_date()`, `omop_last_date()` and `omop_history()` return
  ordinary reducer variables; `before`, `after` and `window` are mutually
  exclusive and default to any time up to the index date; a numeric `unit`
  restricts the unit, `omop_unit()` converts.
- Parameters (design section 2.10): `omop_param(name, type, default,
  description)` placeholders stand for any language-2 value (integer and
  number placeholders admit `+ - * /`, unary minus and unary plus); `recipe_params()`
  lists them and refuses two definitions of one name; `recipe_bind()` checks
  each value against its type, substitutes it and rebuilds every object that
  held a placeholder through its constructor (a recipe may be bound in
  stages); `recipe_to_plan()` binds defaults and refuses an unbound
  parameter. Parametrised recipes round-trip through JSON, YAML and
  `recipe_to_code()`. Acceptance recipe 13 and its twelve variants bind to
  their frozen plans.
- `recipe_merge(a, b, prefix = NULL)`: one shared namespace (a name used by
  both needs one definition), or every node of `b` renamed `<prefix>_<name>`
  with every reference rewritten (anchors, censoring and its index-source
  tags, index arms, parents, set operations, match inputs, output rows,
  columns and dependencies, publication roots); outputs with implicit
  columns keep exactly their own variables, `b`'s base population keeps its
  meaning, block metadata is preserved and parameters are shared by name.
- The complete set of editing functions is exported: `recipe_add_population()`,
  `recipe_add_event()`, `recipe_add_concept_set()`, `recipe_add_interval()`,
  `recipe_add_block()`, `recipe_add_variable()`, `recipe_add_filter()`,
  `recipe_add_output()`, their `recipe_remove_*()` counterparts,
  `recipe_set_scope()`, `recipe_set_options()` and `recipe_set_publish()`
  (2.10.1 formals unchanged).
- `ds.omop.mutate(x, ..., newobj)` (design 2.11, 4.6) derives columns from R
  expressions translated into the closed mutation AST of the vendored
  `mutate-v1.json` -- columns, literals, `+ - * /`, conditions,
  `ifelse()`, `case_when()`, `coalesce()`, `cut()` (R's `right = TRUE`
  default), date arithmetic -- validated before it is sent and never
  evaluated; `newobj = x` coordinates replacement across sites with verified
  rollback and retained recovery copies where restoration is uncertain.
  `ds.omop.filter(x, expr =
  omop_where(...))` filters with the closed predicate grammar. Both need
  the servers to advertise `verb.mutate` / `verb.filter_expr` (dsOMOP
  2.10.3); `omopMutateDS` is a listed server method of
  `ds.omop.server.version()`.

## Corrected
- (A-24) The exact frozen windowed-features corpus fixture `#1` has an
  unindexed `base` and is `unchanged_invalid`. Its explicit v1 omitted-grain
  refusal permits an attempted native rendering, but does not certify that
  the recipe executes: native rendering and automatic selection refuse with
  the indexed-population diagnostic before assignment. Root cohort scope
  alone does not supply an index. A separate indexed, per-variable-window
  variant defaults to episode grain and passes server semantic validation;
  its execution regression remains gated on native compiler integration.

- (WS-9 review B1–B2; A-23) Standalone verb assignments include commit
  verification in their transaction. Fresh outputs are removed on every site
  after a verification failure. In-place mutations restore every possibly
  written site, including a site with a lost acknowledgement; recovery copies
  remain where restoration cannot be confirmed. Errors distinguish unresolved
  recovery, failed cleanup and a mutation that has already committed.
- (WS-9 review B6–B8; A-23) Prefixed recipe merges preserve the original
  namespace at each deferred parameter occurrence, including defaults,
  inline concept sets, window anchors, censor tags, population/output references
  and publication roots, through binding and serialization. A whole-source
  placeholder remains intact in generated advanced code.
  Parameterized zero washout generates the same criteria as literal zero.
  Base sharing is resolved after binding when necessary, using current
  population and source definitions and preserving edits made after merging.
  Generated advanced code preserves edited block members and provenance;
  simple new-user and study printing states the chosen washout and era defaults.
- (WS-9 re-review R3–R4; A-23) Simple helpers defer numeric unit restrictions,
  new-user event selection and row-count expansion until parameter binding.
  Defaults, staged binding, prefixed merges and JSON/YAML/generated-code forms
  retain the literal helper's plan, source options and user edits. Predicate
  serialization preserves numeric literal storage. Parameter arithmetic uses
  numeric intermediates, avoiding integer-storage overflow while retaining
  contextual integer validation and non-finite-expression refusals.
- Large numeric literals and public cut boundaries use round-tripping JSON
  numbers in verb and plan transport, preserving distinct representable values
  beyond the range of exact consecutive doubles.
- `ds.omop.mutate()` and `ds.omop.filter(expr = )` check every participating
  site's published protocol and verb method before any assignment, including
  an in-place mutation's rollback copy. Old, incomplete or unavailable
  profiles refuse the whole call with the missing methods and upgrade advice;
  advertised verb capabilities alone do not bypass this check. The legacy
  `var`/`op`/`value` filter keeps its existing route.
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
- `recipe_to_code()` of a recipe with exactly one language-2 variable emitted
  `variables = name = ...`, which does not parse; it is `list(name = ...)`.
- A refused manipulation verb (`ds.omop.merge()`, `ds.omop.filter()`,
  `ds.omop.select()`, `ds.omop.bind_rows()`, `ds.omop.mutate()`) reports each
  server's own reason -- for a publication refusal its constant message --
  instead of the server names or DSI's "There are some DataSHIELD errors".

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
  (A-14/A-24) and needs an indexed population; an explicit incompatible grain
  (such as `options = list(grain = "person")`) keeps the ordinary v1 refusal,
  as does the corresponding `wide` grain refusal. An explicit episode grain
  retains v1 selection when the complete recipe is v1-expressible.
- (design 2.2, 6.12) Survival outcomes and interval or temporal-covariate
  sources may be narrowed by visit or concept column; an intervals output
  may mix concept-scoped and unscoped variables of one table (one source per
  scope); a temporal-covariates output may declare different analyses per
  concept (per-source analyses, A-14).
- These shapes have no v1 rendering: they run on dsOMOP 2.10.3 servers;
  `recipe_lint()` reports them as INFO `language_2_only`.
- `ds.omop.filter(expr = omop_where(...))` filters a server-side table with
  every comparison of the predicate grammar (`< <= > >=`, `between`,
  `is.na()`, `!`, `&`, `|`), three-valued; the legacy `var`/`op`/`value`
  form keeps `==`, `!=`, `in`, `not_in`.

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

- Adding a conflicting definition for an existing variable-block ID is
  refused; re-adding the identical definition leaves the recipe unchanged.
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
  site's profile publishes (review B1). With every site on dsOMOP 2.10.3 the
  publication protocol runs (`omopPlanStatusDS`, `omopStagedRevokeDS`,
  `commit_policy`). While any site lacks it, a language-2 plan is
  refused before any assignment, naming the
  sites, the missing methods and the required installed/published version
  (dsOMOP 2.10.3). A v1-selected recipe or plan then runs on every site
  as the legacy call of dsOMOPClient 2.10.2 (the same v1 payload, no
  `commit_policy`, no status or revocation calls) with one warning naming the
  old sites: `on_small_site` does not change that legacy call; cross-site
  rollback and the section 7.3 publication guarantees
  do not apply until they upgrade, and each server applies its own
  disclosure rules. DP preparation and the sticky query library follow the
  same rule. A site whose profile cannot be listed, or that lacks a method
  the call needs on either route, stops the execution before anything is
  sent. `ds.omop.server.version()` reports both installed and published
  versions; an Opal profile retains its previous methods until publication.
- With a 2.10.1 or 2.10.2 client and a 2.10.3 server, a strict publication refusal
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
  Amendment A-20 (j) adds `stockpile_row_cap` and `reducer_row_cap` (the
  server's bounded interval realisations) to the vocabulary.

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
