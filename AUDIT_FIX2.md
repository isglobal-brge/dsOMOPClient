# Audit repair 2: implemented corrections and limits

These changes are development work for 2.10.5. Full CR-32/A-31 acceptance remains
open; this document does not certify the release or upstream equivalence.

Event and concept-set declarations named `meta`, `metadata`, or
`circe_provenance` require exactly the same imported-vocabulary capability and
preflight checks as any other name. An incapable server refuses the plan; there
is no fallback to native vocabulary semantics. Actual envelope `meta` and
population `circe_provenance` remain advisory.

`recipe_circe_check()` reports independently unsupported measurement bounds,
occurrence settings, windows, age and sex clauses even when several occur in
one definition. Diagnostics retain structural paths and bounded remedies;
arbitrary values or unknown field names are not echoed. The importer invokes
the same checker and never returns a weakened recipe.

Editing an imported population's legacy index or filters now rebuilds its
cached native translation through the supported local bridge. This corrects
stale translations silently overriding the edit. An explicit native `index`
chooses the native declaration. If both the legacy declaration and its cached
translation were independently edited, compilation refuses with
`ambiguous_circe_edit`: choose an explicit native index, or remove `circe_v2`
to use the edited legacy declaration. Keep the original imported object if
original phenotype parity matters; an edited recipe is a derivative.

Save/load, generated code and prefixed merging preserve original source bytes
and hashes. Merging refreshes derivative metadata immediately. This evidence
covers the tested population edits; it does not certify all recipe-context,
slicing, codebook or provenance obligations in Q03/Q11. The A-30 codebook and
A-29 numeric prerequisite APIs are not installed in this audit branch.
