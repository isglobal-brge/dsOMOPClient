# dsOMOPClient 2.10.8 (2026-11-19)

- With dsOMOP 2.10.8 servers, automatic vocabulary lists are adaptive: each
  server climbs the vocabulary hierarchy only as far as DataSHIELD's rule
  needs, so many observed diagnoses publish with the concepts of a few groups
  instead of a whole terminology.
- Long public lists travel as fingerprints: servers send their list's digest,
  the client reads each distinct list once, and servers holding the same list
  rebuild the shared levels from it (checked by digest). Naming provenance stays
  on the client. Results equal the full transport, which older servers keep.
- With dsOMOP 2.10.8 servers, the level rule counts a person's repeated value
  once: a person-level variable repeated on the event rows of a long output,
  stacked or joined, is weighed against its people, as in a wide output. A
  long output whose observed levels passed only through such repetition now
  gets its automatic vocabulary list, or is refused with the rule named.
- Corrected: `ds.omop.filter()` and `ds.omop.bind_rows()` work on reconciled
  recipe outputs and keep their shared levels, and `ds.omop.table()` and
  `ds.omop.drop_empty_levels()` work on their results (needs dsOMOP 2.10.8).

# dsOMOPClient 2.10.7 (2026-11-19)

- With dsOMOP 2.10.7 servers, a concept column with more observed levels than
  DataSHIELD's rule admits publishes without any recipe change when a short
  public vocabulary list holds its values: the servers declare it themselves.
  Month, quarter and week dates (and year factors) without `level_range`
  declare every period of their observed years. A list longer than a server's
  `dsomop.auto_declared_levels_max` is refused, and the message names that
  option with the remedies: group the codes, narrow the events, or declare the
  codes studied with `omop_concept_levels()`.
- New `ds.omop.drop_empty_levels()`: a copy for models whose factor columns keep
  only the levels that hold a value at some site, identical everywhere, so a
  pooled `ds.glm()` has no all-zero design column. Each site reports its levels
  in use under DataSHIELD's level rule; a column that fails it somewhere keeps
  all its levels, with a message to group it first. It works on reconciled
  outputs (a `recipe_execute()` result), and says so when a declared reference
  category was empty everywhere and the first kept category becomes the
  reference. Needs dsOMOP 2.10.7.
- REAL/NUMERIC birth columns give ages and dates with dsOMOP 2.10.7 servers.
- Faster reconciliation of long level lists: resolving the shared levels and
  their naming decisions is linear in the number of levels (it was quadratic).

# dsOMOPClient 2.10.6 (2026-11-18)

- Concept IDs are categories everywhere; DataSHIELD's factor-level rule.
  Every published concept column is a factor with identical levels across
  retained sites, including untranslated, staged, joined and reducer outputs.
  `translate_concepts = FALSE` now chooses decimal ID labels in numeric ID
  order: compare labels such as `"4306655"`, not numbers.
  `factor_concepts = TRUE` remains accepted; `factor_concepts = FALSE` is gone.
  Publishing plan outputs, catalog assignments and row operations requires dsOMOP 2.10.6 or
  later on every retained server: older untranslated and loader contracts
  cannot prove complete concept-column coverage. Aggregate analyses and
  explain-only inspection remain available on supported older servers.
  Republish source tables retained with raw concept columns before using
  standalone row operations.
- Observed level discovery follows dsBase 6.3.5 `asFactorDS1`: the site's
  distinct non-declared levels must fit `nfilter.levels.max` and
  `nfilter.levels.density` times its output rows, including missing values.
  Per-level `nfilter.tab` support and the shared-union cap no longer apply.
  A stricter rule protects nothing: `ds.asFactor` releases the same levels
  under this rule, and means of published per-concept 0/1 flags already give
  exact counts. A looser rule would expose levels DataSHIELD refuses.
  Count/table `nfilter.tab`, subset, DP and sensitive-column rules are unchanged.
- Declared levels need no discovery and have no level limit: explicit category
  levels, complete recodes, binary flags, IDs in label maps, complete public
  hierarchy domains and concept sets resolved on the public vocabulary do not
  reveal site occurrence. New `omop_concept_levels()` expands an
  `omop_concept_set()` on the servers' vocabulary tables into declared levels for
  `omop_category_spec(levels = )`. Events restricted by a concept set declare it
  for their concept column automatically. Kept unmapped values remain observed.
  Complete recodes can group rare source concepts before publication.
  Refusals name the column, site and rule without counts, explain how to
  declare/group levels or narrow events, mention that the server administrator
  can raise `nfilter.levels.max` / `nfilter.levels.density`, and revoke the
  publication.
  Legacy untagged outputs also refuse unsafe reconciliation instead of retaining
  numeric concept columns. Shared level assignment has no level-count rule.
- Exploration with many concept codes: with dsOMOP 2.10.6 servers,
  `ds.omop.value.counts()`, `ds.omop.crosstab()` and concept drilldowns return
  the codes with enough persons even when a concept column has more than
  `nfilter.levels.max` distinct codes; rare codes stay hidden, and date columns
  keep their level gate.
- A refused publication whose reason is `concept_level_rule` explains the
  remedies: declare the concepts with a concept set, group them, or ask that
  server's administrator to raise the limits.
- Plan executions and catalog assignments declare `concept_protocol = 1`: this
  client reconciles every published concept factor. dsOMOP 2.10.6 servers
  refuse untranslated legacy plans from older clients, which would leave level
  sets that differ between servers. Catalog loaders keep concept IDs as text
  codes on 2.10.6 servers.

# dsOMOPClient 2.10.5 (2026-11-15)

- Release: paired server/client 2.10.5, with checked numeric expressions,
  typed categories and shared codebooks, versioned clinical formulas and
  recorded-day reduction. Republish server methods and Options after upgrading;
  retain existing private roots. Broader workflow/declaration-library work is
  deferred.

Release validation: [Opal contract run 43661271157](https://github.com/davidsarrat/dsomop-opal-e2e/actions/runs/43661271157)
and [end-to-end run 43661274062](https://github.com/davidsarrat/dsomop-opal-e2e/actions/runs/43661274062).
The release pipeline records both as successful; the end-to-end artifacts cover
fresh installation and upgrade from 2.10.4 on Opal 6.2.1, including preservation
of the existing DP noise root. These runs do not establish live execution on
every SQL dialect or exercise the separate numeric scenario N.

- Corrected: ordinary `any`/`all` reducers still require a predicate at
  construction. Within `omop_category_reduce()`, the typed category input
  supplies that context, so indicator reductions remain available without an
  unrelated event predicate. No process-wide validation option is used.

- Corrected: mixed named and unnamed variable declarations pass through category
  bundle expansion until normal recipe naming resolves them; genuine expanded
  name collisions still refuse. Existing declaration syntax remains valid.
- Corrected: Circe export preflight preserves precise refusals for half-open
  numeric ranges and authenticated safe-bin contracts. Keep these filters in
  native recipes; neither can be represented faithfully by the Circe bridge.

- Corrected (STG-R1-01): staging cleanup validates every committed descriptor
  before classifying expired or consumed output. Parseable malformed manifests
  preserve the entire directory and its ownership tracking. Re-run affected
  recipes after upgrading; public readers still reject invalid or expired data.

- Newly permitted (CR-33 / A-32): repeated staged executions no longer stop at
  a per-handle directory count. Before reserving a directory, the server reclaims
  only handle-owned staging whose committed outputs are all expired, consumed
  (data files removed), or revoked. Live outputs and active executions remain
  intact; byte and row caps still protect staging. Re-run affected recipes;
  `dsomop.max_staging_dirs_per_handle` is retired and ignored.

- Corrected (A-29 second review): integer numeric expressions retain a
  conservative signed64 result type through nested formulas, selectors and
  publication, including empty inputs. Re-run affected recipes; integer
  expression results now consistently use integer64 storage.
- Newly refused: native numeric arithmetic and mixed numeric branches/coalesce
  reject inexact integer-to-binary64 conversions even for inline intermediates.
  Retain raw/count identity representation or use exactly representable inputs;
  naming an intermediate does not change admission. Legacy mutate semantics
  and the closed expression grammar are unchanged.

- Corrected (shared codebook presentation): prospective columns use the execution
  numeric/date/category evaluators on empty public prototypes, including existing
  and generated dates, canonical numeric copies and categorical copies. Membership
  provenance and mixed covariate ordering share their execution declarations.
  Rebuild existing codebooks; recipe syntax does not change.
- Corrected (cross-format equivalence): requested plain numeric covariates no
  longer disappear without an explicit number specification, mixed covariates
  follow declared order, and untranslated reference tables retain the provenance
  needed for codebook reconciliation. Existing recipes gain their requested
  columns; downstream code should select columns by their declared names.
- Corrected (native multi-state identity): restore lossless episode keys after
  state-machine evaluation, including keys above 2^53 and empty results. Native
  row keys are character strings; use them as opaque identifiers rather than
  converting them to floating point. The v1 state machine is unchanged.

- Corrected (membership/long codebook review): prospective existing date columns
  honor recipe category defaults, with producer-specific applicability. Membership
  schemas also include the declared roster's index provenance in publication order.
  Native row keys are declared as the lossless character strings actually released.
  Rebuild affected codebooks; recipe declarations and published data do not change.

- Corrected (generated date factor review): prospective codebooks apply recipe
  category defaults to generated date endpoints, including ordering, recodes,
  public levels, reference and contrasts in nested and joined outputs. Rebuild
  affected codebooks; recipe declarations and published data do not change.

- Corrected / newly refused: CKD-EPI 2021 eGFR rejects input-local source-unit
  maps that differ from the age anchor's shared event normalization. Such maps
  could select creatinine from a different date and use the wrong calendar age
  or adult eligibility. Move `omop_unit()` normalization onto the event shared
  by creatinine and `omop_age_at()`; retain explicit numeric coordinate
  conversions on the creatinine input. Aligned shared maps and day-zero/index
  age inputs remain supported.

- Newly permitted: four exact-version client formula constructors (BMI,
  arithmetic MAP, ratio, and race-free CKD-EPI 2021 creatinine eGFR) expand into
  existing typed private operations. Declare source selection, canonical units
  or explicit conversions, and the eGFR sex mapping/calendar-age anchor. Invalid
  values yield typed NA; structural unit/anchor/version errors refuse. Formula
  identity and derivative provenance survive JSON, YAML and generated R.
- Newly permitted: `omop_number_daily()` selects original events, applies input
  presentation, collapses within recorded concept-days, then applies the outer
  numeric reducer and final presentation. Mean/max/first and skip/propagate are
  explicit. Use this route for equal-day weighting instead of ordinary record
  weighting. It requires the server's `number.daily` capability; old servers
  refuse before assignment. No absent days, reference ranges or support counts
  are invented.
- Corrected: YAML and generated R retain small binary64 coefficients exactly,
  including the eGFR age factor. Regenerate affected serialized recipes from
  their original numeric declarations; already rounded coefficients cannot be
  recovered from saved text.
- Scope: broader A-31 deliverables are deferred as listed in
  the issue tracker. Real Opal evidence is cited above; numeric scenario N was not exercised there.

- Corrected (audit prerequisite integration): imported vocabulary validity
  remains executable in category rollup domains, including exclusions.
  Use a server advertising vocabulary.ohdsi_import_validity for these domains.
- Corrected: prospective generated-date columns apply category ordering defaults
  consistently with publication. Rebuild affected codebooks.

- Corrected: explicit coverage on an imported Circe population now overrides
  the translated coverage default during compilation, including saved and
  generated recipes. Remove the explicit coverage to restore the imported
  policy. Original source provenance is retained and edits remain derivatives.
- Corrected: Circe inspection reports independent unsupported clauses inside
  nested groups at each structural path; groups remain unsupported for import.

* **Corrected:** Circe inspection reports measurement bounds, occurrence,
  window, age and sex clauses independently when several are unsupported.
  Editing an imported legacy population now rebuilds its native translation;
  an explicit native index takes precedence. Conflicting edits to both stored
  representations are **newly refused**: choose a native index or remove
  `circe_v2` to execute the edited legacy declaration. Prefixed merges refresh
  derivative provenance and retain original source hashes.

* **Corrected:** imported-vocabulary capability and preflight traversal now
  distinguishes declaration registries from advisory metadata. Event and
  concept-set names such as `meta`, `metadata`, and `circe_provenance` retain
  identical validation and capability requirements. Existing names remain
  valid; use a capable server and valid vocabulary references for these plans.

* **Corrected / newly refused:** both OHDSI import paths attach the executable
  `ohdsi-import-1` vocabulary policy. Explicit included/excluded concepts must
  exist as valid public vocabulary records (including concept 0); invalid
  descendants and invalid mapped sources are excluded. Mapping follows valid
  Maps-to sources toward selected targets; classification concepts are allowed.
  Native declarations retain their existing behavior. Re-import old saved
  definitions to opt into the stricter policy. Policy-bearing plans require
  the paired server's `vocabulary.ohdsi_import_validity` capability; incapable
  servers refuse without native fallback. Save/load, code generation, merge,
  wire normalization and semantic digests retain the policy.

* **Newly permitted:** `recipe_circe_support()` describes the directional
  bridge subset, and `recipe_circe_check()` inspects definitions locally and
  reports independent unsupported clauses with bounded structural diagnostics.
  Import/export use the checker. Native populations without an export adapter
  receive `unsupported_native_export`; retain their native recipe.
* **Corrected:** Circe imports retain original JSON bytes (canonical JSON for
  list inputs), hashes, adapter/subset versions and optional sanitized public
  origin declarations. Save/load retains the origin and marks edited population
  declarations as derivatives. This provenance is client-side, not a claim of
  general phenotype equivalence. Vocabulary validity is enforced separately by
  the executable policy described above.
  The pinned PhenotypeLibrary 304 example has a private membership/date oracle
  against its upstream SQL; unsupported example 10 reports structural refusals.
  Re-import an old definition to obtain the new provenance fields.

* **Newly refused:** incidence `clean_window` values other than zero and
  `fe.prevalence` `time_windows` values other than zero now fail with a bounded
  setting-specific remedy before patient computation on ordinary, protected
  and alias routes. These selectors previously did not implement the requested
  science. Use a validated native risk/window recipe when that setting matters;
  dropping a required window is not an equivalent analysis. Explicit null,
  nonfinite/fractional values and malformed selectors are invalid settings.
* **Corrected:** incidence discovery describes the first-target-episode,
  full-window person-day estimand and separates contribution bounds from risk
  windows. FE prevalence describes all-history events among scoped members,
  without a cohort-relative date predicate.

* **Newly refused:** both OHDSI concept importers now validate concept IDs and
  Boolean flags before coercion, exclusion or deduplication. Supply integral
  JSON numbers in 0..2147483647 (including 0 for unknown), not numeric strings;
  use true/false or omit flags, not null. Duplicate keys and unsupported item
  semantics refuse. Nested concept descriptions are limited to CONCEPT_NAME,
  STANDARD_CONCEPT, STANDARD_CONCEPT_CAPTION, INVALID_REASON,
  INVALID_REASON_CAPTION, CONCEPT_CODE, DOMAIN_ID, VOCABULARY_ID,
  CONCEPT_CLASS_ID, VALID_START_DATE and VALID_END_DATE; they never affect
  membership. Circe cohort exclusions/mixed flags remain unsupported; the
  registry retains uniform included/excluded groups. List and JSON/file inputs
  use the same strict parser.

- Corrected (date/category schema review): prospective codebooks include native
  date endpoints before final selection, with their effective presentation types
  and order, including nested survival and joined outputs. Rebuild affected
  codebooks; recipe declarations and published data do not change.

- Corrected (categorical schema review): prospective codebooks enumerate closed
  panel source columns and apply the same required numeric-role rules as
  execution for selected panel and survival copies, including nested components.
  Rebuild affected codebooks; no recipe declaration changes are needed.

- Corrected (categorical composition review): final column selection and order
  apply to output-local copies and expanded variables in both execution and
  prospective codebooks. Structural columns remain present.
- Corrected: output-local category copies accept categorical selector reductions
  (including mode and vector nth), retaining consumed identity, missingness and
  admission contracts while allowing independent presentation. Re-run affected
  native recipes; no declaration changes are needed.

- Corrected (categorical review): prospective codebooks expand native nth-value
  columns and honor effective numeric, date and category presentation defaults,
  overrides and untranslated concepts. Native outputs can select expanded
  keys such as `answer_3` in the requested order.
- Corrected: recipe hierarchy defaults are inactive for semantic flags and
  numeric/date categories, including `multiple="all"`. Explicit incompatible
  hierarchy requests still refuse; use concept inputs and category indicators
  when multiple hierarchy targets are intended.

- Native language-2 releases now carry codebook metadata for every real table,
  including plain numeric and untranslated plans. They require
  `output.codebook_metadata`; upgrade participating servers to 2.10.5.
  Existing v1 plans retain their compatibility path.

- Newly permitted (A-30 / CR-31): native category specifications add typed
  identity order, nominal recodes, hierarchy maps, language and code labels,
  explicit Missing/Unknown levels and semantic-flag representations. Category
  event projections, indicators and reductions preserve explicit identity.
- Newly permitted: declared repeated time-point/visit columns and output-local
  category copies; client codebooks use public plans and admitted release
  metadata. These constructs require native language 2 and matching 2.10.5
  capabilities. Use wide/long typed columns for factor analyses, with a numeric
  sibling for quantitative arithmetic.
- Newly refused: ambiguous hierarchy mappings require a declared priority or
  membership indicators. A mixed zero/nonzero user-label merge cannot become
  an Unknown sentinel: retain unknown=keep, use separate labels, or explicitly
  recode its identities before sentinel handling.

- Corrected: native matrix and person-period outputs no longer implicitly
  consume unrelated recipe variables. Select analytic columns explicitly;
  use a typed wide companion for factor/logical matrix covariates.
- Corrected: undefined anchors remain NA through repeated subsetting and
  Missing presentation. Staged empty category contracts preserve nullable
  references, declared levels and contrast specifications.
- Newly permitted: prospective codebooks describe structural schema and
  survival/reference component families locally; published codebooks retain
  reconciled names, category provenance and mapping contracts.
- Categorical binary presentation of abnormal-event reducers means at least
  one abnormal event. Keep an unwrapped numeric sibling for event counts.

- Corrected (A-29 review): independently normalized numeric siblings retain
  their original event-row binding in long and intervals outputs. BIGINT
  longitudinal arithmetic now checks exact conversion before calculation;
  retain raw/count identity when a binary64 conversion is refused.
- Corrected: expression types follow each selected canonical/presented input
  prefix, preserving fractional values and exact integers. Slope reducers and
  generated relation-day columns retain class-2 provenance through copies,
  categories and transport. Supply roles use days; unrelated numeric roles
  do not inherit measurement units. Re-run affected numeric recipes.
- Newly refused: scalar formulas in long output, including nested formulas,
  require explicit `broadcast = TRUE`, just like direct scalar reducers.

- Corrected: shared numeric contracts retain full binary64 precision through
  DataSHIELD JSON transport, including fractional conversion factors and
  adjacent breakpoints. Re-execute recipes to reconcile those contracts.

- Newly permitted (A-29 / CR-30): native numeric presentation with declared
  source and fixed units, ordered transforms, exact binary64 decimal rounding,
  public bins and clinical categories, flags and value presence/missingness.
  Numeric rows, counts, longitudinal changes, closed multi-input expressions
  and output-local canonical column copies run before protection and release.
  Numeric capabilities are negotiated before assignment; new declarations
  require native language 2 and dsOMOP 2.10.5 at every selected site.
- Newly permitted: numeric factor reconciliation retains the complete ordered
  public domain, including unused levels. Memory and Parquet carry numeric
  units, expression identity, domains and privacy lineage. Incompatible retained
  contracts fail and roll back; a numeric sibling is required for arithmetic.
- Corrected: A-29 CV results are dimensionless SD/mean. The compatibility route
  without numeric declarations retains its percentage CV. To display a native
  CV as a percent, explicitly apply an affine factor of 100.
- Corrected: exact BIGINT selectors and integer count promotion avoid silent
  binary64/32-bit loss. Identity and round(none) retain integer64; operations
  requiring an inexact double conversion refuse with a bounded remedy.
- Newly refused: explicit numeric presentation of identifiers, concept values,
  Dates/factors, model weights or structural codes; factor-valued matrix cells;
  implicit event broadcasts; and nonnumeric/foreign expression bindings.
  Use registered numeric measures, explicit numeric date representations,
  declared broadcasts/reducers, and typed wide/long companions as applicable.

- Corrected with the matching server: empty active-comparator populations now
  receive `insufficient_support` at the final gate. Default execution rolls back
  all sites; `on_small_site = "skip"` retains both analysis roots at supported
  sites, in memory and staged modes. This also covers populations emptied by
  eligibility criteria. Update the server and re-run the recipe.

- Fixed deferred panel source policy names during prefixed recipe merging,
  so binding before or after a merge produces the same plan.

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

The release candidate passed recipe/publication and end-to-end checks for fresh installations and upgrades on Opal 5.7.6 and 6.1.3. These checks do not establish live execution on every supported SQL dialect.

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
