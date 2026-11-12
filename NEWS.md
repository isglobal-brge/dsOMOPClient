# dsOMOPClient 2.10.3

Pairs with dsOMOP 2.10.3, the security release of the OHDSI-tools audit.

- New protected-release contract: dsOMOP 2.10.3 servers advertise
  `privacy_contract = "protected_release_fixed_basis_or_realised_v1"`.
  `ds.omop.dp.release()` validates their `dsomop-protected-release-v1`
  payloads (route, guarantee scope, signed components, no private material,
  no history-dependent fields) and pools them by summing the signed components
  of compatible sites and deriving each view once. A federation mixing 2.10.3
  servers with per-release servers (2.7.1-2.10.2) is refused; per-release
  federations keep the previous behavior.
- Heuristic payloads are never presented as differential privacy:
  `realised_sticky` releases report `mechanism_epsilon = NA`, a warning, and no
  pooled epsilon; `meta$privacy$protection` names each site's route. For
  fixed-synopsis releases the pooled epsilon is reported for disjoint sites
  (maximum) and overlapping sites (sum).
- Standalone typed counts on 2.10.3 servers use the shared person-total
  allocation (0.2 of the reference calibration) and are noisier than before;
  typed bounded means need a registered public scale; typed histogram breaks
  are reported as requested and as effective (public-grid) breaks; a
  distinct-label cardinality is not pooled across sites.
- `ds.omop.analysis.run()` pools protected catalog releases (the
  `release_pooling` descriptor of 2.10.3 entries) from their signed components
  (one row per owner and stratum: persons, mean, SD, events, person-time and
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
  synopsis without a finite mechanism epsilon and delta 0. Only fixed
  synopses are printed with a delta.

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
