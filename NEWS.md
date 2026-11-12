# dsOMOPClient 2.10.3 (in development)

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
- `on_small_site = "skip"` on plan and recipe execution keeps successful sites
  while asking each server to commit all output families or none. Non-strict
  partial results warn with rejected family names and codes.
- Numeric row thresholds and one-sided measurement criteria compile without
  safe bins; supplied bin contracts retain their scope checks. Removed the
  `narrow_filter` / `unbound_numeric_filter` lints and filter safety categories.
- Circe round-trips one-sided measurement bounds with identical endpoint
  semantics; half-open two-sided ranges and authenticated bin contracts still
  refuse export because Circe cannot preserve them.

## Newly refused
- Invalid or incomplete publication status fails closed; only
  `insufficient_support` can skip a site.
- Plans that do not validate against the vendored `plan-v2.json` are never
  sent; a v2 recipe whose operations an older server lacks is refused with
  the operation named.

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
