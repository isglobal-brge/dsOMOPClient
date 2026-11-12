# dsOMOPClient 2.10.3

Pairs with dsOMOP 2.10.3, the security release of the OHDSI-tools audit.

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

Recipe language 2.0 (design-2.10.3). This section is the WS-0 skeleton; each
workstream fills its entries.

## Corrected
- Plan execution reads authenticated publication status before cleanup and uses
  the server inventory, including freshly allocated component symbols. Technical
  and transport failures revoke staged files as well as assigned outputs.
- A partial `out` mapping without an explicit `publish` requests only the
  mapped outputs. For example, `out = c(conditions = "cond_df")` publishes
  only `cond_df`; the server leaves other computed outputs private and the
  client accepts the matching publication status.
- (B-8) `parent_id` now restricts membership (see dsOMOP 2.10.3).
- (F-3) The client blocklist is identical to the server's class-1 patterns;
  birth components are not blocked on assignment paths. Informational
  `quasi_identifier` lint identifies birth, age and date variables without
  changing compilation or aggregate admission.

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
  the operation named.

## Shared with dsOMOP 2.10.3 (WS-2)
- The language 2.0 normal form is the frozen, byte-identically vendored
  `R/plan_normalize_v2.R` (`.planNormalizeV2()`, amendment A-11); the
  server's `.irNormalize()` calls the same function, so normalised forms and
  semantic digests agree on both sides. WS-2's interim `R/ir_defaults.R`
  (`.irNormalizeWire()`) is removed.
- The role-registry tables (`inst/ohdsi/roles-5.3.csv`, `roles-5.4.csv`) are
  vendored byte-identically from dsOMOP.

## Upgrading clients and servers
- Plan execution with dsOMOPClient 2.10.3 requires the server's published
  `omopPlanStatusDS` and `omopStagedRevokeDS` methods; dsOMOP 2.10.1 does not
  provide them. Upgrade the server to 2.10.3 and check
  `ds.omop.server.version()`. On Opal, publish dsOMOP's settings again after
  upgrading so the profile exposes the new methods.
- With a 2.10.1 client and a 2.10.3 server, a strict publication refusal
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
