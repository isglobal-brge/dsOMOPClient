# dsOMOPClient 2.10.3 (2026-11-13)

## Date presentation (A-28 / CR-29)

- Corrected (DATES-B4): year-bearing custom factors include every year in a
  declared `level_range`, including unused middle years and ISO boundary weeks.
  Empty or disjoint observed dates no longer cause valid public keys to refuse
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

## Recipe language 2 and concept presentation

This release includes the recipe-language-2 work. The rewritten
population and extraction vignettes teach the simple and advanced layers,
including coded education level and cause of death. Examples remain
`eval = FALSE` until integrated compilers pass acceptance.

## Corrected


- (A-27 review F2/P1) `ds.omop.table()` preserves arbitrary permitted labels,
  including commas, through temporary protected analysis copies with lossless
  category tokens. Counts map back to the exact public level contract, including
  unused declarations and deliberate merges; actual missing values stay missing.
  Existing disclosure checks remain in force. Use matching server/client builds
  and this helper for free-text pooled tables; direct dsBase label transport
  remains comma-delimited. No recipe changes are needed.

- (A-27 review fixes) CSV staging preserves literal `"NA"` labels separately
  from actual missing cells, including reconciliation rewrites. New CSV
  descriptors declare `csv_encoding = "backslash-v1"`; server-side readers
  must decode through `.publishReadChunks()` (a leading backslash escapes
  literal `NA` and leading backslashes). Existing descriptors retain their
  legacy decoding. Unicode whitespace-only recodes are now rejected on both
  boundaries; replace them with a nonempty label. Valid text is unchanged.

- Use `ds.omop.table(data, column, conns)` for a complete pooled concept table.
  It integrates the real `dsBaseClient::ds.table()` counts with the existing
  permitted level contract, restoring globally unused declarations as zero
  cells. Direct dsBase calls still omit those categories. Actual missing
  values are excluded; failed or suppressed site counts cause refusal.

- (A-27) Concept factors preserve their federation level contract during
  model-frame subsetting, including locally absent categories in `ds.glm`.
  Translated legacy dictionaries use the declared concept schema in both
  memory and staged execution; they do not expose an observed ID inventory.

- (A-27/CR-28) Concept-valued results are unordered factors by declared
  semantic class, including aliased reducers and nested/staged references.
  Vocabulary labels and generated concept names reconcile by ID across
  retained sites; missing names fall back to decimal IDs, duplicate automatic
  labels/names are disambiguated, and absent categories retain zero-count
  levels. Computations and joins still use internal IDs. For numeric IDs
  and untranslated generated names, use
  `options = list(translate_concepts = FALSE)`.


- Execution reads authenticated status before deleting its execution symbol,
  uses the committed dynamic inventory and verifies cleanup of assigned and
  staged artefacts. A partial `out` mapping without explicit `publish` releases
  only mapped outputs; an explicit empty publication set is preserved.
- Parent populations constrain membership. The shared normalizer and semantic
  digest agree with the server; version semantics are bound and advisory names
  are excluded. Safe v1 behavior and legacy positional arguments are retained.
- Class-1 blocking agrees with the server. Birth, age and date variables receive
  informational quasi-identifier lint without changing aggregate admission.
  Concept-valued wide outputs are translated by declared type, including
  renamed education/cause columns, through the A-27 presentation contract.
- Parameters, simple-study expansion and recipe composition retain declared
  semantics. Native indexed windowed features may default to episode grain;
  the exact unindexed legacy fixture remains invalid (A-24).

## Newly permitted


- (A-27/CR-28) Append `labels` and `column_names` to `omop_variable()` and
  `omop_recipe()`. Variable maps override recipe maps. Equal explicit value
  labels deliberately merge categories; explicit column names address stable
  untranslated keys and never merge columns. Maps require native rendering.
  Presentation-only federations may use different vocabulary versions;
  comparable newest releases win, otherwise UTF-8 byte order breaks ties.

- `omop_study()` and shortcuts return advanced recipe objects; printing states
  defaults and `recipe_to_code()` reproduces the expansion. Events, reducers,
  eras, anchors, coverage, matching and named output components use language 2.
- Assigned exact birth components and age; off-grid age bands, narrow windows,
  numeric predicates and private small intermediates. Aggregate policy stays
  unchanged. Coded education and death cause use concept-valued wide columns;
  free text remains class 1 even when declared categorical.
- `on_small_site = "skip"` retains passing sites and asks a small site to commit
  all families or none. Non-strict `"error"` returns partial results with a
  warning naming rejected roots by code. Only support failures are skippable.
- Closed mutation expressions and expression filters, subject to advertised
  operations and actual DataSHIELD method classifications. In-place mutations
  retain recoverable originals until federation commit/restoration is verified.
- Role-registry index tables, table-domain concept filters and one-sided numeric
  criteria; supported Circe round-trips retain endpoint semantics. Unsupported
  half-open/bin-contract exports still refuse explicitly.

## Newly refused

- (A-27/CR-28) Translation with `factor_concepts = FALSE`, malformed maps,
  invalid or colliding effective explicit names, old servers without
  `output.concept_labels`, and incomplete/disclosure-unsafe shared factor
  contracts fail clearly. Remove `factor_concepts = FALSE` or explicitly
  disable translation. Reconciliation failures roll back the federation;
  `on_small_site = "skip"` cannot bypass them.


- Malformed or incomplete publication status and technical/transport failures
  fail closed with cleanup, rather than being mistaken for skipped sites.
- V2 plans that fail the vendored schema, unsupported operations on an older
  server, protected-key/class-1 mutation references and invalid grain/index
  combinations fail before assignment. Unknown recipe fields are not ignored.
- Recipe-level row filters cannot attach to an output whose event table lacks
  their columns; put the predicate on the appropriate `omop_event(where = ...)`.
- Adapter errors remain errors under both small-site policies, with the bounded
  `validation_error` reason shown alongside site and output. They cannot skip
  a site as though it merely lacked final support.

## Upgrading clients and servers

Use matching dsOMOP/dsOMOPClient 2.10.3 versions. Republish dsOMOP's methods and
Options on Opal after upgrading, then call `ds.omop.server.version()`.
`omopPlanStatusDS` and `omopStagedRevokeDS` are required for plan execution;
2.10.1 servers lack them and language-2 operations. A 2.10.1 client can still
run safe v1 plans on the new server, but a refused plan can surface as missing
requested outputs. The Opal O1–O12 matrix tests both compatibility directions
and the installation/republishing path. README separates required dialect
support from runtime evidence; native operations remain capability-gated.

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
