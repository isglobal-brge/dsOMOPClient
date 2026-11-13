# OHDSI QueryLibrary Sticky Redesigns

The QueryLibrary integration distinguishes an upstream clinical question
from its published SQL. The SQL is training/reference material and is
never executed by the sticky redesign path. Instead, a typed recipe or
plan defines the population and variables, the server prepares a
person-local `omop.table` sealed for DP, and a pinned mapping fixes one
of the seven sticky primitives and its longitudinal reducer.

The roles are those of DataSHIELD: the analyst chooses the question (the
population, the derived variables such as an age at a public reference
year, and the public bins or levels); the data custodian owns every
privacy setting (whether DP is available, epsilon, caps and the
community tools’ protection mode). No client argument changes those.

## Automatic questions in 2.10.4

Common reviewed questions need no explicit preparation. With the default
`dsomop.tools.mode = "auto"`, the catalog routes protected
distributions, record summaries and effects through their registered
component contracts. Record summaries such as `condition.duration_stats`
use capped record-weighted components (up to ten records per person) and
public scales; unregistered scales retain histograms while moments
remain unavailable. Demographic partitions use a complete public domain.
The protection label identifies fixed synopses, heuristic
realised-contribution reuse, or a mixed frame. No known-disclosive exact
fallback is used when the protected route is unavailable.

``` r
res <- ds.omop.analysis.run("dsomop:condition.duration_stats",
                            params = list(concept_id = 201820))
res$pooled             # views derived from compatible pooled signed components
res$meta$protection    # per-site route, allocations and unavailable fields
```

Questions with an automatic equivalent list it in
`omop_querylibrary_sticky_catalog()$automatic_equivalent` and when
printed.

## Sticky redesigns

The core never uses differential privacy (dsOMOP 2.8.0 and later):
tables loaded with `ds.omop.plan.execute()` or `recipe_execute()` carry
no DP provenance, and the server refuses them as DP inputs. A sticky
redesign is released on a table prepared for DP from a typed recipe or
plan (`ds.omop.dp.prepare()`, dsOMOP 2.9.1 or later). The release
prepares it for you when the redesign carries its recipe.

This is deliberately not a QueryLibrary SQL runner or an arbitrary
join/query gateway. An upstream ID selects only a reviewed semantic
mapping; it neither submits the upstream SQL nor constructs a
data-dependent query. Preparation and release remain two explicit, typed
steps.

The client cannot choose epsilon, seed, nonce, privacy epoch or rerolls.
Servers 2.10.3 and 2.10.4 report `privacy_contract =
"protected_release_fixed_basis_or_realised_v1"`. Fixed-synopsis releases
share a registered public basis; realised-contribution reuse is
heuristic and has no mechanism epsilon. Signed components are pooled
only when the contracts and logical source identities are compatible.
Old per-release payloads cannot be pooled with this contract; retain
per-site results during an incompatible upgrade and upgrade both
packages together.

Noise is generated deterministically on demand, with no stored answers,
lifetime budget, query quota or privacy accountant. Exact support gates,
overlapping scopes and evolving data prevent a general full-transcript
DP claim. See the security chapter and server disclosure documentation
for the scope and utility of each protected route.

``` r
catalog <- omop_querylibrary_sticky_catalog()
table(catalog$statistic)
#>
#>           binary_rate      bounded_distinct          bounded_mean
#>                     7                     1                     7
#>  bounded_record_count categorical_histogram                 count
#>                     4                    31                     8
#>     numeric_histogram
#>                    71
```

Every category domain, numeric/date break, bound, concept set, time
window, ordering rule and record cap must be public and fixed before
data access. Observed categories and data-dependent top-N selection are
not valid inputs.

For example, PE12 can be redesigned as a coarse
age-at-a-fixed-public-year distribution. This avoids placing exact birth
components in the prepared table and uses public decade breaks:

``` r
age_at_2025 <- omop_querylibrary_sticky(
  "PE12",
  variable = "age_at_2025",
  breaks = seq(0, 120, by = 10)
)
age_at_2025
#> OHDSI QueryLibrary sticky redesign: PE12
#>   Primitive: numeric_histogram
#>   Reducer: mean
#>   Input: a table prepared with ds.omop.dp.prepare()
#>   Automatic equivalent: ds.omop.analysis.run("dsomop:person.year_of_birth_distribution") (no preparation)
#>   Upstream SQL authorized: no
```

Give the redesign its recipe and release it in one call; the input is
prepared on every server, released, and removed:

``` r
recipe <- omop_recipe(
  variables = omop_variable_age(name = "age_at_2025", year = 2025),
  outputs = omop_output(name = "ages", type = "wide")
)
age_at_2025 <- omop_querylibrary_sticky(
  "PE12", variable = "age_at_2025", breaks = seq(0, 120, by = 10),
  preparation = recipe
)
result <- ds.omop.querylibrary.sticky.release(age_at_2025, datasources = conns)
```

The same step can be explicit, for example to release several statistics
on one prepared table:

``` r
ds.omop.dp.prepare(recipe, out = c(ages = "DP_ages"))
result <- ds.omop.querylibrary.sticky.release("DP_ages", age_at_2025,
                                              datasources = conns)
```

The whole release population must be defined in the recipe or plan:
filtering, merging or otherwise manipulating a prepared table drops its
DP provenance. Preparation requires person-local lineage. Matching
descendants and unsealed external frames are refused; an assigned age or
date alone does not certify DP provenance. Use the explicit preparation
route for the intended population.

Longitudinal numeric and categorical record histograms use a
deterministic declared order and a finite per-person record cap:

``` r
monthly_conditions <- omop_querylibrary_sticky(
  "CO19",
  variable = "month_number",
  breaks = seq(0.5, 12.5, by = 1),
  max_contributions = 12,
  order_by = "month_number"
)
```

Scalar record totals and site-local database-wide distinct drug
cardinality are also available only as explicitly capped estimands. The
latter uses a complete public vocabulary-derived concept domain and
canonical server-owned ordering:

``` r
condition_records <- omop_querylibrary_sticky(
  "CO10", max_contributions = 25
)

distinct_rxnorm <- omop_querylibrary_sticky(
  "DEX06",
  variable = "drug_concept_id",
  levels = public_standard_rxnorm_ids,
  max_contributions = 50
)
```

DEX06 is a site-local bounded distinct cardinality. It is not pooled
across sites: there is no privacy-safe cross-site linkage to compute the
concept union from those counts. Use `pool = FALSE` to retain
site-specific cardinalities.

Exact ZIP, source/free-text labels and patient/event rows remain
unavailable. They do not silently fall back to a different query:

``` r
omop_querylibrary_sticky("PE08")
#> Error:
#> ! QueryLibrary PE08 is blocked: exact ZIP geography.
```

Use `omop_querylibrary_sticky_catalog(include_unavailable = TRUE)` to
inspect the complete 201-ID partition: 129 executable bounded redesigns,
54 vocabulary/reference metadata questions, and 18 blocked result
shapes.
