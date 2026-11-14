# Categories and repeated columns (A-30)

These APIs require matching dsOMOP and dsOMOPClient 2.10.5 with native
language-2 plans. Category keys identify meanings; factor storage codes do not.

```r
education <- omop_variable(
  event = omop_event("observation", 45000120L),
  reducer = omop_reduce("last_value", value = "value_as_concept_id"))
keys <- c("concept:45000123", "concept:45000124", "concept:45000125")
education <- omop_category(education, omop_category_spec(
  levels = keys, order = omop_category_order(keys), ordered = TRUE))
recipe <- omop_recipe(variables = list(education = education))
recipe_codebook(recipe)  # prospective; no connection or data query
```

The IDs in this example are synthetic fixture IDs. Use the study's declared
public concepts in an actual recipe. A wrapper overrides explicit variable
fields, which override recipe defaults. Each nested object is replaced as a
whole. `NULL` inherits; `ordered=FALSE`, identity order, an empty recode map,
and `rollup(kind="none")` are explicit resets.

For a nominal factor, `reference="concept:45000124"` declares the treatment
reference independently of its display position. Ordered factors carry
polynomial contrasts. Keep a numeric sibling when a model needs a numerical
scale; never analyze factor codes as measurements. Endpoint admission and
supported model types still apply.

Rollups accept public concept keys or `omop_concept_set()` domains and use
vocabulary tables only. ATC levels select exact classes 1–5. ICD10 chapter
and block classification uses reverse valid Maps to edges and ICD10 hierarchy
edges; it does not reconstruct a recorded source code. SNOMED separation is
exact minimum or maximum distance. Ambiguity defaults to refusal: declare a
complete priority or request `multiple="all"` with category indicators.
Use `omop_category_reduce()` over explicit event category rows when mapping
must happen *before* mode/distinct/count, rather than after selecting a value.
Reduction consumes that input stage once. An explicit outer category wrapper
can request another applicable recode, hierarchy stage, or vocabulary style
on the resulting identity.

```r
lab <- omop_event("measurement", 3000963L, name = "hemoglobin")
points <- omop_timepoints(list(
  baseline = omop_timepoint(),
  m6 = omop_timepoint(6, "months", tolerance = c(-7, 7)),
  m12 = omop_timepoint(12, "months", tolerance = c(-7, 7))))
columns <- omop_repeated(lab, list(value = omop_number_rows(lab)),
                         timepoints = points)
recipe <- omop_recipe(events = list(lab),
  populations = omop_population("p", index = omop_index(lab)),
  variables = list(lab = columns),
  outputs = omop_output("wide", format = "wide", rows = "p"))
```

Months clamp to the target month's last day; a leap-day plus one year maps
to March 1. Each target is computed from the original anchor. Ties use earlier
date, source namespace, and exact event-key order. `reuse=FALSE` allocates
rows in declared point order. Paired values select one row using their
canonical operand missingness; presentation cannot change that selection.
`omop_visits(c(1,2,3), source=omop_event("visit_occurrence"))` ranks registered
visits first, then joins measurements by their recorded visit key. An empty
second visit remains the second visit.

Missing levels apply to ordinary available NA. Unavailable sources and
undefined anchors remain typed NA. Only effective concept zero has built-in
Unknown semantics. An explicit recode can consume zero into an ordinary
category. Semantic flags can be integer, logical, or factor; ordinary numeric
0/1 values do not acquire flag identity by their observed contents.

`label_style` selects name, code, code-name, or vocabulary-code strings.
`omop_category_language(language_concept_id)` chooses requested-language
synonyms deterministically, falling back to the ordinary vocabulary name.
Explicit user labels take precedence. Source-code display does not authorize
access to class-1 source fields.

After execution, `recipe_codebook(result)` uses only the release metadata
already admitted for each published component. It does not query a server,
report counts or missingness, or retry rejected components. Keep the result
receipt or use the bound recipe for a prospective codebook.

Plain native language-2 releases also carry codebooks, including untranslated
numeric outputs. Every participating server must advertise
`output.codebook_metadata`. Nested survival components receive their explicitly
selected covariates and their own schema records. Prospective books preserve
public structure while leaving automatic vocabulary names and open domains
unresolved. Codebooks do not say whether a declared level was observed.

Intentional user-label merges retain their canonical minimum-ID identity and
permitted aliases through category reduction. A later recode may name an alias,
but cannot split its existing equivalence group. Conflicting alias targets
refuse. A group containing zero and a known concept must be recoded as a whole
before requesting Unknown classification.

Arithmetic after a semantic flag clears its flag status. A later explicit
threshold/presence/missing operation restores it. For an available empty time
point, raw and threshold results are NA, present is 0 and missing is 1; an
unavailable source or undefined/overflowing target remains NA throughout.

Protected table and GLM analyses use the existing in-memory endpoint routes.
`ds.omop.table()` retains declared unused categories and pools counts by the
shared identity contract; model routes use the declared contrasts. Parquet
chunks retain the same types, full levels and contrasts for downstream staged
consumers. Factor codes are never a replacement for an explicit integer flag.
