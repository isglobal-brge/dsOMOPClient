# Vendored 2.10.3 plan schemas

Byte-identical copies of `dsOMOP/inst/schema/` (the server package owns them;
`SHA256SUMS` lists the same checksums). `ir-v2.json` is server-only and is not
vendored. Regenerate on the server with `tools/ws0/gen_schema_artefacts.py`
and copy; `test-ws0-schema.R` in both packages pins the identity.

A-28/CR-29 adds closed native calendar, parts, elapsed and age declarations.
The shared date semantics and normalizer resolve fields after inheritance;
unsupplied legacy date policies stay absent on native columns.
