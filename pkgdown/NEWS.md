# dsOMOPClient 2.10.9

Use matching dsOMOP and dsOMOPClient packages for the study examples in this book.
Publish the installed server methods and options in the selected Opal profile,
then check its published and running versions from the client.

Concept IDs are categories everywhere, with the same levels at every site.
Observed categories follow DataSHIELD's factor-level rule (a person's repeated
value counts once); declared lists
(`omop_concept_levels()`, recodes, label maps, concept sets) need no discovery.
When a site observes more categories than the rule admits, the servers declare
a public list that holds them, climbing the vocabulary's hierarchy only as far
as needed; dates by month or week declare every period of their observed years. Before a pooled
model, `ds.omop.drop_empty_levels()` drops levels that no site uses.

Tables work as in dsBase: `ds.omop.table()` returns `ds.table()`'s object, per
site and combined, also for two-way tables (`by =`). Published labels keep only
letters, digits, `.`, `_`, `-` and `+`, so that every dsBase function can carry
them on Opal; the codebook keeps the exact names. Categories held by fewer than
`nfilter.tab` people at a site can be made missing for descriptive tables
(`ds.omop.drop_small_categories()`, `recipe_execute(small_categories =
"missing")`, `ds.omop.table(small = "missing")`); see
[What stays private](../articles/book-12-privacy.html#tables-when-some-categories-have-very-few-people).
Calls follow dsBaseClient's conventions: `datasources =` for the connections,
and `type = "split"`, `"combine"` or `"both"` for per-site and pooled results.

The package also provides numeric units, rounding, bins, thresholds,
transformations, change over time and clinical formulas. Categories support
declared ordering, labels, hierarchy groups and missing-value policies. Each
output has a codebook that records its column types and presentation rules.

Start with [Getting started](../articles/book-00-start.html), explore the
[worked examples](../articles/book-showcase.html), or read the
[complete package release history](https://github.com/isglobal-brge/dsOMOPClient/blob/main/NEWS.md).
